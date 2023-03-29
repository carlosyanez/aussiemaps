library(aussiemaps)
library(fs)
library(sf)
library(here)
library(tidyverse)

source(here("R","internal.R"))
source(here("R","cache_management.R"))
source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))


vic_file <- here("data-raw","2021_Victoria.gpkg")
vic <- st_read(vic_file,quiet=TRUE)

vic <- vic |> mutate(id=str_c(id,"-",STATE_CODE_2021))

vic_structure <- vic
vic_structure$area <- st_area(vic_structure)
vic_structure <- vic_structure |> st_drop_geometry()


structure <- list_structure(year=2021) |>
             filter(STATE_CODE_2021!="2") |>
             bind_rows(vic_structure)

file_delete(vic_file)

st_write(vic,vic_file,layer="2021_Victoria")

save_zip_gpkg(vic_file,
              here("data-raw"),
              here("data-raw","processed"))

save_zip_parquet(structure,str_c("2021_structure"),here("data-raw","processed"))

geo_cols <- colnames(structure)
geo_cols <- c(geo_cols[str_detect(geo_cols,"CODE")],"POA_NAME_2021")

for(geo_col in geo_cols){
  struct_i <- structure %>%
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

