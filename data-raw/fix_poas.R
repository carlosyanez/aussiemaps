#FIX VICTORIAN POAS

library(tidyverse)
library(sf)
library(arrow)
library(fs)
library(here)
library(zip)
library(piggyback)
library(rgdal)
library(aussiemaps)

source(here("R","internal.R"))
source(here("R","cache_management.R"))
source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))

cache_dir  <- find_maps_cache()
#years <- c(2011,2016,2021)
years <- c(2011,2016)
poas_list <- 3000:3999

geo_files <- c("2021"="ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2022.gpkg",
              "2016" ="asgs2016nonabsstructures.gpkg",
              "2011" ="asgs2011nonabsstructures.gpkg"
               )

geo_files <- here("data-raw","source",geo_files)
names(geo_files) <- c(2021,2016,2011)

geo_layers<- c("2021"="POA_2021_AUST_GDA2020",
               "2016"="post_code_area_2016",
               "2011"="post_code_area_2011")

file_regex <- str_c("[0-9]{4}_[A-Z]{1}")

repo       <- arrow::read_parquet(path(cache_dir,"repo.parquet")) |>
  mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
  select(any_of("file_name"))                                   |>
  filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
  pull()

state_names <-list_attributes() |>
  filter(str_detect(attributes,"STATE|STE")) |>
  filter(str_detect(attributes,"NAME")) |>
  pivot_longer(-attributes,names_to = "year",values_to="colname") |>
  filter(!is.na(colname))

state_codes <-list_attributes() |>
  filter(str_detect(attributes,"STATE|STE")) |>
  filter(str_detect(attributes,"CODE")) |>
  pivot_longer(-attributes,names_to = "year",values_to="colname") |>
  filter(!is.na(colname))

for(year_i in years){

  repo_year <- repo[str_detect(repo,as.character(year_i))]
  state_year <- state_names |>
                filter(year==as.character(year_i)) |>
                pull(colname)
  state_year_code <-  state_codes |>
    filter(year==as.character(year_i)) |>
    pull(colname)

  state_filter <- as.vector("Victoria")
  names(state_filter) <- state_year

  vic_map <- get_map(filters=state_filter,year=year_i)

  poa_col <- colnames(vic_map)[str_detect(colnames(vic_map),"POA")]


  vic_map_with_poas <- vic_map |> filter(if_any(all_of(poa_col), ~!is.na(.x)))
  vic_map_without_poas <- vic_map |>
                          filter(if_any(all_of(poa_col), ~is.na(.x))) |>
                          select(-any_of(c(poa_col,"AREA_ALBERS_SQKM","ASGS_LOCI_URI_2021")))

  filename  <- geo_files[names(geo_files)==year_i]
  file_year <- geo_layers[names(geo_layers)==year_i]
  poa_col_name <-poa_col[str_detect(poa_col,"NAME")]
  poa_col_code <-poa_col[str_detect(poa_col,"CODE")]


  poas <- st_read(filename,layer=file_year) |>
          filter(if_any(c(poa_col), ~ .x %in% poas_list))

  full_overlap <- full_coverage(base=vic_map_without_poas,
                                bigger=poas,
                                base_id="id",
                                bigger_id=poa_col_name)

  base_renmant <- vic_map_without_poas  %>%
    filter(!(id %in% full_overlap$id))


  intersects <- intersections(base_renmant,
                              bigger=poas,
                              base_id="id",
                              bigger_id=poa_col_code,
                              base_empty_label=str_c("SA2_NAME_",year_i),
                              bigger_empty_label=poa_col_name)

  non_matched <- vic_map_without_poas %>%
    filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                intersects %>% st_drop_geometry() %>%
                                                  select(id) %>%
                                                  pull()
    )))))

  vic_map_without_poas <- vic_map_without_poas %>%
    left_join(full_overlap,by="id") %>%
    filter(if_any(poa_col_name, ~ !is.na(.x)))


  vic_map_without_poas <- bind_rows(vic_map_without_poas,intersects)
  vic_map_without_poas <- bind_rows(vic_map_without_poas,non_matched)


  vic_map <- bind_rows(vic_map_with_poas,vic_map_without_poas)

  state_code <- vic_map[1,] %>% st_drop_geometry() |>
                select(any_of(state_year_code)) %>% pull()

  vic_map <- vic_map %>% mutate(id=state_code,"-",row_number())

  vic_file <- here("data-raw",str_c(year_i,"_Victoria.gpkg"))
  st_write(vic_map,vic_file)
  vic_map <- st_read(vic_file)
  save_zip_gpkg(file=vic_file,
                source_dir=here("data-raw"),
                dest_dir=here("data-raw","processed"))

  #redo year's structure

  vic_map$area <- st_area(vic_map)
  vic_structure <- vic_map |> st_drop_geometry() |>
                  select(-any_of(c("AREA_ALBERS_SQKM","X...","row_number...")))

  year_structure <- list_structure(year_i) |>
                    filter(if_any(c(state_year), ~ .x!="Victoria")) |>
                    bind_rows(vic_structure) |>
                    select(-any_of(c("AREA_ALBERS_SQKM","X...","row_number...")))

  save_zip_parquet(year_structure,str_c(year_i,"_structure"),here("data-raw","processed"))

  #clean POAS_CODES

struct_i <- vic_structure %>%
  filter(!is.na(area)) %>%
  select(any_of(c(poa_col_code,"id","area"))) %>%
  rename("geo_col"=poa_col_code) %>%
  group_by(geo_col) %>%
  mutate(sum_area = sum(area)) %>%
  ungroup() %>%
  mutate(prop=if_else(sum_area>units::set_units(0,m^2),
                      as.numeric(area/sum_area),
                      0))


poa_codes <- load_aussiemaps_parquet(str_c(poa_col_code))

poa_codes <- poa_codes |>
  collect() |>
  filter(!(geo_col %in% struct_i$geo_col)) |>
  bind_rows(struct_i)

save_zip_parquet(poa_codes,poa_col_code,here("data-raw","processed"))



}
