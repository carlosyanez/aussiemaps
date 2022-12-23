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
years <- c(2006,2011,2016,2021)

file_regex <- str_c("[0-9]{4}_[A-Z]{1}")

repo       <- read_parquet(path(cache_dir,"repo.parquet")) |>
  mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
  select(any_of("file_name"))                                   |>
  filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
  pull()

for(year in years){
  repo_year <- repo[str_detect(repo,as.character(year))]

  geo_structure <- NULL

  for(map in repo_year){
    map_i <- load_aussiemaps_gpkg(map)
    #map_i <- st_read(path(cache_dir,str_c(map,".gpkg")))

    state_col <- colnames(map_i)[str_detect(colnames(map_i),"STATE|STE")]
    state_col <- state_col[str_detect(state_col,"CODE")]
    state_col_pos <-which(colnames(map_i)==state_col)
    state_code <- map_i[1,state_col_pos] |> st_drop_geometry() |> pull()

    map_i <- map_i |>
             mutate(id=str_c(state_code,"-",row_number())) |>
             mutate(Year=year) |>
             mutate(across(contains("CODE"),as.character))

    file_gpkg <- str_c(map,".gpkg")
    st_write(map_i,here("data-raw",file_gpkg),layer=map)

    save_zip_gpkg(here("data-raw",file_gpkg),
                  here("data-raw"),
                  here("data-raw","processed"))

    map_i$area <- st_area(map_i)
    map_i <- map_i %>% st_drop_geometry()
    geo_structure <- bind_rows(geo_structure,map_i)

  }

  geo_structure <-  geo_structure |>
    relocate(id,.before=1)

  save_zip_parquet(geo_structure,str_c(year,"_structure"),here("data-raw","processed"))


  geo_cols <- colnames(geo_structure)
  geo_cols <- geo_cols[str_detect(geo_cols,"CODE")]


  attributes <- geo_structure[1,] %>%
    select(-area,-Year) %>%
    pivot_longer(-id,names_to="attributes",values_to = "value") %>%
    select(attributes)


  save_zip_parquet(attributes,str_c(year,"_attributes"),here("data-raw","processed"))

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


}

for(year in years){
  geo_structure <- load_aussiemaps_parquet(str_c(year,"_structure"))

  geo_cols <- geo_structure$schema$names
  geo_cols <- geo_cols[str_detect(geo_cols,"CODE")]

  for(geo_col in geo_cols){
    struct_i <- geo_structure %>%
      select(any_of(c(geo_col,"id","area"))) %>%
      collect() %>%
      filter(!is.na(area)) %>%
      rename("col"=geo_col) %>%
      group_by(col) %>%
      mutate(sum_area = sum(area)) %>%
      ungroup() %>%
      mutate(prop=if_else(sum_area>units::set_units(0,m^2),
                          as.numeric(area/sum_area),
                          0)) %>%
      rename(geo_col="col")

    save_zip_parquet(struct_i,geo_col,here("data-raw","processed"))
    print(str_c(which(geo_cols==geo_col)," out of", length(geo_cols),": ",geo_col)
          )


  }



}

