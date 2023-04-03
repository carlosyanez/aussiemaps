library(tidyverse)
library(sf)
library(fs)
library(arrow)
library(leaflet)
library(aussiemaps)
library(sfarrow)



source(here("data-raw","functions.R"))
source(here("R","internal.R"))

dont_write <- TRUE

main        <- here("data-raw","source","ASGS_2021_Main_Structure_GDA2020.gpkg")
nonabs      <- here("data-raw","source","ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2022.gpkg")
indigenous  <- here("data-raw","source","ASGS_Ed3_2021_Indigenous_Structure_GDA2020.gpkg")
other       <- here("data-raw","source","ASGS_2021_SUA_UCL_SOS_SOSR_GPKG_GDA2020.gpkg")

main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
indigenous_layers <- rgdal::ogrListLayers(indigenous)
other_layers      <- rgdal::ogrListLayers(other)


temp_file <- here("data-raw","temp_df.parquet")
base_file <- here("data-raw","base_sf3.parquet")
ced_file <- here("data-raw","source","divisions-Aug-2021-by-2016-SA1")
threshold <-0

## South Australia ----
state       <- "South Australia"
state_short <- "Tas"

poas_state <- c(5000:5799,0872)
ceds_2018 <- auspol::list_divisions(filters=list(StateAb="SA",`2022`=TRUE)) %>% pull(DivisionNm)

data_base <- load_aussiemaps_gpkg("2021_South.Australia.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

sa1 <- b[i,]
sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

existing <- data_base |> filter(SA1_CODE_2021==sa_code)
existing <- tibble(a="sa_code",geom=st_union(existing)) |>
  st_as_sf() |>
  smoothr::fill_holes(units::set_units(1,"km^2"))


base_i <- st_difference(sa1,existing)
base_i <- st_cast(base_i, "POLYGON")

base_i$area <- st_area(base_i)
base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
if(is.null(base)){
  base <- base_i
}else{
  base <- bind_rows(base,base_i)
}

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## Western Australia ----
state       <- "Western Australia"
state_short <- "Wa"
ceds_2018 <- list_divisions(filters=list(StateAb="WA",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(6000:6797,0872)

data_base <- load_aussiemaps_gpkg("2021_Western.Australia.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## NT ----
state       <- "Northern Territory"
state_short <- "Nt"
ceds_2018 <- list_divisions(filters=list(StateAb="NT",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(0800:0899,4825)

data_base <- load_aussiemaps_gpkg("2021_Northern.Territory.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## Queensland ----
state       <- "Queensland"
state_short <- "Qld"
ceds_2018 <- list_divisions(filters=list(StateAb="QLD",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(4000:4999,2406)

data_base <- load_aussiemaps_gpkg("2021_Queensland.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## NT ----
state       <- "Northern Territory"
state_short <- "Nt"
ceds_2018 <- list_divisions(filters=list(StateAb="NT",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(0800:0899,4825)

data_base <- load_aussiemaps_gpkg("2021_Northern.Territory.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)

## NSW ----
state       <- "New South Wales"
state_short <- "Nsw"
ceds_2018 <- list_divisions(filters=list(StateAb="NSW",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2000:2599,2619:2899,2921:2999,2406,2540,2611,3585,3586,3644,3691,3707,4380,4377,4383,4385)

data_base <- load_aussiemaps_gpkg("2021_New.South.Wales.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## Victoria----
state       <- "Victoria"
state_short <- "Vic"
ceds_2018 <- list_divisions(filters=list(StateAb="VIC",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- 3000:3999

data_base <- load_aussiemaps_gpkg("2021_Victoria.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)




## Tasmania----
state       <- "Tasmania"
state_short <- "Tas"

poas_state <- 7000:7999
ceds_2018 <- list_divisions(filters=list(StateAb="ACT",`2022`=TRUE)) %>% pull(DivisionNm)

data_base <- load_aussiemaps_gpkg("2021_Tasmania.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  base_i <- st_difference(sa1,existing)
  base_i <- st_cast(base_i, "POLYGON")

  base_i$area <- st_area(base_i)
  base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

  message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))


rm(base)



