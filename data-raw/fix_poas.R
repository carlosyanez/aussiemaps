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
years <- c(2011,2016,2021)

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

  }
}
