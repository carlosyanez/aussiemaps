library(tidyverse)
library(sf)
library(fs)
library(arrow)
library(leaflet)
library(aussiemaps)
library(sfarrow)
library(auspol)
library(here)



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

  tryCatch({
    base_i <- st_difference(sa1,existing)
    base_i <- st_cast(base_i, "POLYGON")

    base_i$area <- st_area(base_i)
    base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

    message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))

  },
  error = function(e){
    message("same?!")
  })
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
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)

base |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = "orange")


rm(base)




## NSW ----
state       <- "New South Wales"
state_short <- "Nsw"
ceds_2018 <- list_divisions(filters=list(StateAb="NSW",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2000:2599,2619:2899,2921:2999,2406,2540,2611,3585,3586,3644,3691,3707,4380,4377,4383,4385)

data_base <- load_aussiemaps_gpkg("2021_New.South.Wales")

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

  tryCatch({
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
  },
  error = function(e){
    message("same?!")
  })
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

base <- base |> st_make_valid()
base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]

base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
base <- base |> st_cast("POLYGON")

st_write(base,here("data-raw",str_c("2021_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


#rm(base)

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

  existing <- data_base |> filter(SA1_CODE_2021==sa_code) |> st_make_valid()
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    st_make_valid() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))


  tryCatch({
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
  },
  error = function(e){
    message("same?!")
  })

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- st_cast(base,"POLYGON")
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))

base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


#rm(base)


## Tasmania----
state       <- "Tasmania"
state_short <- "Tas"

poas_state <- 7000:7999
ceds_2018 <- list_divisions(filters=list(StateAb="TAS",`2022`=TRUE)) %>% pull(DivisionNm)

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

  tryCatch({
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
  },
  error = function(e){
    message("same?!")
  })

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- st_cast(base,"POLYGON")

base <- bind_rows(data_base |> select(-any_of(c("id"))),
                  base |>  select(-any_of(c("id"))))

st_write(base,here("data-raw",str_c("2021_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


rm(base)




## ACT----
state       <- "Australian Capital Territory"
state_short <- "Act"
ceds_2018 <- list_divisions(filters=list(StateAb="ACT",`2022`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2600:2618,2900:2920,2620,2540)


data_base <- load_aussiemaps_gpkg("2021_Australian.Capital.Territory.gpkg")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)

data_base$empty <- data_base |> st_is_empty()
data_base <- data_base |> filter(!empty) |> select(-empty)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)

data_base <- st_cast(data_base,"POLYGON")
b <- st_cast(b,"POLYGON")

base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)

  if(nrow(existing)>0){

  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))

  tryCatch({
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
  },
  error = function(e){
    message("same?!")
  })
  }
  else{
    base_i <- sa1
    if(is.null(base)){
      base <- base_i
    }else{
      base <- bind_rows(base,base_i)
    }

  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-any_of(c("a","area")))
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))

base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))

rm(base)



## Other Territories----
state       <- "Other Territories"
state_short <- "Other"
poas_state <- c(2540,2899,6798,6799,7151)

ceds_2021_out <- load_geo(nonabs,"CED_2021_AUST_GDA2020") %>%
  filter(CED_NAME_2021 %in% c("Fenner","Bean","Lingiari")) %>%
  st_drop_geometry() %>%
  select(CED_CODE_2021,CED_NAME_2021) %>%
  left_join(tribble(~ SA3_NAME_2021,~CED_NAME_2021,
                    "Christmas Island","Lingiari",
                    "Norfolk Island","Bean",
                    "Jervis Bay","Fenner",
                    "Cocos (Keeling) Islands","Lingiari",
                    "Migratory - Offshore - Shipping (OT)","Migratory - Offshore - Shipping (OT)",
                    "No usual address (OT)","No usual address (OT)"
  ),by="CED_NAME_2021")



data_base <- load_aussiemaps_gpkg("2021_Other.Territories")
b         <-  load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){

  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_CODE_2021)

  existing <- data_base |> filter(SA1_CODE_2021==sa_code)

  if(nrow(existing)>0){

    existing <- tibble(a="sa_code",geom=st_union(existing)) |>
      st_as_sf() |>
      smoothr::fill_holes(units::set_units(1,"km^2"))

    tryCatch({
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
    },
    error = function(e){
      message("same?!")
    })
  }
  else{
    base_i <- sa1
    if(is.null(base)){
      base <- base_i
    }else{
      base <- bind_rows(base,base_i)
    }

  }

}

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))

if(nrow(base)>0){
base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
}else{
  base <- data_base
}


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

  if(nrow(existing)>0){

    existing <- tibble(a="sa_code",geom=st_union(existing)) |>
      st_as_sf() |>
      smoothr::fill_holes(units::set_units(1,"km^2"))

    tryCatch({
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
    },
    error = function(e){
      message("same?!")
    })
  }
  else{
    base_i <- sa1
    if(is.null(base)){
      base <- base_i
    }else{
      base <- bind_rows(base,base_i)
    }

  }

}


base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-any_of(c("a","area")))

keep_vars <- unique(c(ls(),"keep_vars"))

source(here("data-raw","2021","sequence_2021.R"))


existing_poas <- data_base |>
                 st_drop_geometry() |>
                  distinct(SA1_CODE_2021,POA_CODE_2021)

existing_poas <- existing_poas |>
                  left_join(existing_poas |>count(SA1_CODE_2021) |> filter(n==1),by="SA1_CODE_2021" ) |>
                  filter(!is.na(n)) |>
                  select(-n) |>
                  rename("postcode"="POA_CODE_2021")

base <- base |> left_join(existing_poas,by="SA1_CODE_2021") |>
  mutate(POA_CODE_2021=case_when(
    is.na(POA_CODE_2021) ~ postcode,
    TRUE ~ POA_CODE_2021
  ),
  POA_NAME_2021=POA_CODE_2021) |>
  select(-postcode)



base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))

st_write(base,here("data-raw",str_c("2021_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)



map |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = "orange")

rm(base)



#-------

### Correct save, get prop tables -----

source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))

files <- dir_ls(here("data-raw"),regexp = "gpkg")
geo_structure <- NULL

### ACT-----

file <- files[1]

map <- st_read(file) |>
       mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure <- map |> st_drop_geometry()

#### NSW ----
file <- files[2]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))

map <- map |> mutate(POA_CODE_2021=case_when(
 is.na(POA_CODE_2021) & SAL_NAME_2021=="Cottonvale" ~ "4375",
 TRUE ~ POA_CODE_2021),
 POA_NAME_2021=POA_CODE_2021)

st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### NT ----
file <- files[3]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()

nt_postcodes <- tibble::tribble(
                      ~SAL_NAME_2021, ~postcode,
                  "Newcastle Waters",    "0862",
                          "Larrimah",    "0852",
                       "Daly Waters",    "0852",
                        "Koolpinyah",    "0822",
                           "Araluen",    "0870",
                         "Mataranka",    "0852",
                      "Kaltukatjara",    "0872",
                           "Ilparpa",    "0873",
                         "Maranunga",    "0822",
                            "Nauiyu",    "0822",
                        "Gunbalanya",    "0822",
                      "Wagait Beach",    "0822",
                          "Sandover",    "0872",
                       "West Arnhem",    "0822",
                          "Lajamanu",    "0852",
                     "Bynoe Harbour",    "0822",
                      "Dundee Beach",    "0840",
                      "Dundee Downs",    "0840",
                     "Dundee Forest",    "0840",
                      "Micket Creek",    "0822",
                   "Van Diemen Gulf",    "0822",
                            "Kakadu",    "0822"
                  )

map <- map |>
  left_join(nt_postcodes,by="SAL_NAME_2021") |>
  mutate(POA_CODE_2021=case_when(
  is.na(POA_CODE_2021)  ~ postcode,
  TRUE ~ POA_CODE_2021),
  POA_NAME_2021=POA_CODE_2021) |>
  select(-postcode)

st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### Other Territories ----
file <- files[4]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()

#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### Queensland ----
file <- files[5]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()

#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### South Australia ----
file <- files[6]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()

#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### Tasmania ----
file <- files[7]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()
#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### Victoria ----
file <- files[8]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()
#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

### WA ----
file <- files[9]

map <- st_read(file) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number()))
map$area <- st_area(map)

#map|> st_drop_geometry() |> count(POA_CODE_2021) |> filter(is.na(POA_CODE_2021))
#map |> st_drop_geometry() |> filter(is.na(POA_CODE_2021)) |> distinct(SAL_NAME_2021) |> clipr::write_clip()
#all postcodes covered
st_write(map,file,append = FALSE,delete_dsn = TRUE)

save_zip_gpkg(file,
              here("data-raw"),
              here("data-raw","processed"))

geo_structure  <- bind_rows(geo_structure,
                            map |> st_drop_geometry())

geo_structure |> distinct(STATE_NAME_2021)

#save structure, create proportions ----

geo_structure <- geo_structure |>
  relocate(id,.before=1)

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
    mutate(prop=if_else(sum_area>units::set_units(0,m^2),
                        as.numeric(area/sum_area),
                        0)) %>%
    rename(geo_col="col")

  save_zip_parquet(struct_i,geo_col,here("data-raw","processed"))


}

