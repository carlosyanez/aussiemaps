library(aussiemaps)
library(tidyverse)
library(here)
library(fs)
library(sf)

source(here("R","internal.R"))
source(here("R","cache_management.R"))
source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))


cache_dir  <- find_maps_cache()

missingPOAS <- aussiemaps::list_structure(year=2021) |>
               filter(is.na(POA_NAME_2021)) |>
               filter(area >units::set_units(0,"m^2")) |>
               select(id,SAL_NAME_2021,STATE_NAME_2021) |>
               mutate(SAL_NAME_2021=str_to_title(SAL_NAME_2021),
                      SAL_NAME_2021=str_squish(SAL_NAME_2021))


postcodes <- read_csv("https://www.matthewproctor.com/Content/postcodes/australian_postcodes.csv") |>
             select(postcode,locality) |>
             mutate(locality=str_to_title(locality),
                    locality=str_squish(locality))


poas <- missingPOAS |> distinct(SAL_NAME_2021,STATE_NAME_2021) |> left_join(postcodes,
                         by=c("SAL_NAME_2021"="locality"))



missingPOAS <- missingPOAS |>
  left_join(poas |>   filter(!is.na(postcode)) |>
              bind_rows(tibble::tribble(
                ~SAL_NAME_2021,               ~STATE_NAME_2021, ~postcode,
                "Molonglo", "Australian Capital Territory",     "2611",
                "Ku-Ring-Gai Chase",              "New South Wales",     "2084",
                "Thirldene",              "New South Wales",     "2347",
                "Port Stephens",              "New South Wales",     "2319",
                "Palmerston City",           "Northern Territory",      "0830",
                "West Coast",                     "Tasmania",     "7469",
                "Southwest",                     "Tasmania",     "7116",
                "Schouten Island",                     "Tasmania",     "7116"
              )),
            by=c("SAL_NAME_2021","STATE_NAME_2021")) |>
  select(id,postcode) |>
  distinct()

file_regex <- str_c("[0-9]{4}_[A-Z]{1}")


repo       <- arrow::read_parquet(path(cache_dir,"repo.parquet")) |>
  mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
  select(any_of("file_name"))                                   |>
  filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
  pull()

geo_structure <- NULL

for(year in c(2021)){
  repo_year <- repo[str_detect(repo,as.character(year))]


  for(map in repo_year){
    message(map)
    map_i <- load_aussiemaps_gpkg(map)

    map_i <- map_i |> left_join(missingPOAS,by="id") |>
      mutate(POA_CODE_2021=if_else(is.na(POA_CODE_2021),postcode,POA_CODE_2021),
             POA_NAME_2021=POA_CODE_2021) |>
      select(-postcode)

    file_gpkg <- str_c(map,".gpkg")
    st_write(map_i,here("data-raw",file_gpkg),layer=map)

    save_zip_gpkg(here("data-raw",file_gpkg),
                  here("data-raw"),
                  here("data-raw","processed"))

    map_i$area <- st_area(map_i)
    map_i <- map_i %>% st_drop_geometry()
    geo_structure <- bind_rows(geo_structure,map_i)
  }

}

save_zip_parquet(geo_structure,str_c(year,"_structure"),here("data-raw","processed"))

geo_cols <- c("POA_CODE_2021","POA_NAME_2021")

for(geo_col in geo_cols){
  struct_i <- geo_structure %>%
    filter(!is.na(area)) %>%
    select(any_of(c(geo_col,"id","area"))) %>%
    rename("col"=geo_col) %>%
    group_by(col) %>%
    mutate(sum_area = sum(area)) %>%
    ungroup() %>%
    mutate(prop=if_else(sum_area>units::set_units(0,m^2),
                        as.numeric(area/sum_area),
                        0)) %>%
    rename(geo_col="col")

  save_zip_parquet(struct_i,geo_col,here("data-raw","processed"))


}


