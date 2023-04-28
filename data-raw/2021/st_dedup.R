library(tidyverse)
library(sf)
library(fs)
library(arrow)
library(leaflet)
library(aussiemaps)
library(sfarrow)
library(auspol)
library(here)

deduper <- function(data_base){
  equals <- st_equals(data_base)
  data_base$dedup <- 1:nrow(data_base)

  l_dedup <- c()
  for(i in 1:length(equals)){
    if(length(equals[[i]])>1){
      l_dedup <- c(l_dedup,i)
    }
  }

  removed <- c()
  for(l in l_dedup){
    removed <- c(removed,l)

    to_remove <- equals[[l]][!(equals[[l]] %in% removed)]
    data_base <- data_base |> filter(!(dedup %in% to_remove))
  }

  data_base <- data_base |> select(-dedup)

  return(data_base)
}

source(here("data-raw","functions.R"))
source(here("R","internal.R"))

## South Australia ----
state       <- "South Australia"
data_base <- load_aussiemaps_gpkg("2021_South.Australia.gpkg")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)

## NT ----
state       <- "Northern Territory"
data_base <- load_aussiemaps_gpkg("2021_Northern.Territory")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


## Queensland ----
state       <- "Queensland"
data_base <- load_aussiemaps_gpkg("2021_Queensland")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)

## NSW ----
state       <- "New South Wales"
data_base <- load_aussiemaps_gpkg("2021_New.South.Wales")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


## Victoria----
state       <- "Victoria"
data_base <- load_aussiemaps_gpkg("2021_Victoria")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


## ACT----
state       <- "Australian Capital Territory"
data_base <- load_aussiemaps_gpkg("2021_Australian.Capital.Territory")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


## Western Australia ----
state       <- "Western Australia"
data_base <- load_aussiemaps_gpkg("2021_Western.Australia")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


## Other Territories ----
state       <- "Other Territories"
data_base <- load_aussiemaps_gpkg("2021_Other.Territories")
data_base <- deduper(data_base)
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")),delete_dsn = TRUE)


#-------

### Correct save, get prop tables -----

source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))

files <- dir_ls(here("data-raw"),regexp = "gpkg")
geo_structure <- NULL

geo_structure <- bind_rows(st_read(here("data-raw",str_c("2021_New South Wales.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2021_Victoria.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2021_Queensland.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2021_Western Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2021_South Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2021_Northern Territory.gpkg"))) |> st_drop_geometry(),
                           load_aussiemaps_gpkg("2021_Australian.Capital.Territory.gpkg") |> st_drop_geometry(),
                           load_aussiemaps_gpkg("2021_Other.Territories.gpkg") |> st_drop_geometry(),
                           load_aussiemaps_gpkg("2021_Tasmania.gpkg") |> st_drop_geometry())

save_zip_parquet(geo_structure,"2021_structure",here("data-raw","processed"))

geo_cols <- colnames(geo_structure)
geo_cols <- geo_cols[str_detect(geo_cols,"CODE")]

for(geo_col in geo_cols){
  message(geo_col)
  struct_i <- geo_structure %>%
    filter(!is.na(area)) %>%
    select(any_of(c(geo_col,"id","area"))) %>%
    rename("col"=geo_col) %>%
    group_by(col) %>%
    mutate(sum_area = sum(area)) %>%
    ungroup() %>%
    rename(geo_col="col")

  save_zip_parquet(struct_i,geo_col,here("data-raw","processed"))


}

## save files, except NSW
files <- dir_ls(here("data-raw"),regexp = "gpkg")
files <- files[str_detect(files,"Wales",TRUE)]

for(file in files){

  save_zip_gpkg(file,
                here("data-raw"),
                here("data-raw","processed"))
}



