library(tidyverse)
library(sf)
library(fs)
library(arrow)
library(leaflet)
library(aussiemaps)
library(sfarrow)
library(auspol)
library(here)
library(piggyback)



source(here("data-raw","functions.R"))
source(here("R","internal.R"))

dont_write <- TRUE

main        <- here("data-raw","source","asgs2016absstructuresmainstructureandgccsa.gpkg")
nonabs      <- here("data-raw","source","asgs2016nonabsstructures.gpkg")
nonabs2018  <- here("data-raw","source","asgs2016nonabsstructures.gpkg")
indigenous  <- here("data-raw","source","asgs2016absstructuresindigenousstructure.gpkg")
other       <- here("data-raw","source","asgs2016absstructuressignificanturbanareasurbancentresandlocalitiessectionofstate.gpkg")


main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
indigenous_layers <- rgdal::ogrListLayers(indigenous)
other_layers      <- rgdal::ogrListLayers(other)


temp_file <- here("data-raw","temp_df.parquet")
base_file <- here("data-raw","base_sf3.parquet")
ced_file <- here("data-raw","source","divisions-Aug-2016-by-2016-SA1")
threshold <-0

## South Australia ----
state       <- "South Australia"
state_short <- "Sa"

poas_state <- c(5000:5799,0872)
ceds_2018 <- auspol::list_divisions(filters=list(StateAb="SA",`2016`=TRUE)) %>% pull(DivisionNm)

data_base <- load_aussiemaps_gpkg("2016_South.Australia")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016") |>
              filter(STE_NAME_2016==state)

 # sa1_focus <-c("40602114111",
 #               "40602114110",
 #               "40602114008")
 #
 # data_base <- data_base |> filter(SA1_MAINCODE_2016 %in% sa1_focus)
 # b <- b |> filter(SA1_MAINCODE_2016 %in% sa1_focus)


source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))

base$empty <- st_is_empty(base)
base <- base |> filter(!empty) |> select(-empty)

base <- base |>
        st_make_valid() |>
        st_cast("MULTIPOLYGON")


base <- bind_rows(data_base |> select(-any_of(c("id","area"))),
                  base |>  select(-any_of(c("id","area"))))

base <- db_a |>
  filter(!(SA1_MAINCODE_2016 %in% c("40703116412","40602114111","40602114107"))) |>
  bind_rows(base)


base <- base |>
        mutate(id=str_c(STE_CODE_2016,"-",row_number()))

base$area <- st_area(base)

st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn = TRUE)
rm(base)
## NT ----
state       <- "Northern Territory"
state_short <- "Nt"
ceds_2018 <- list_divisions(filters=list(StateAb="NT",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- str_pad(c(0800:0899,4825),4,"left","0")

data_base <- load_aussiemaps_gpkg("2016_Northern.Territory")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016") |>
  filter(STE_NAME_2016==state)

source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))

base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn = TRUE)


rm(base)

## Queensland ----
state       <- "Queensland"
state_short <- "Qld"
ceds_2018 <- list_divisions(filters=list(StateAb="QLD",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(4000:4999,2406)

data_base <- load_aussiemaps_gpkg("2016_Queensland")
b         <- load_geo(main, layer = "statistical_area_level_1_2016")  |>
              filter(STE_NAME_2016==state)

exsa1 <- data_base |> st_drop_geometry() |> distinct(SA1_MAINCODE_2016) |> pull()

base <-b |> filter(!(SA1_MAINCODE_2016 %in% exsa1)) |> rename("geom"="shape")

#b <- b |> filter(SA1_MAINCODE_2016 %in% unique(data_base$SA1_MAINCODE_2016))

source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))



base <- bind_rows(data_base |> select(-any_of(c("id"))),
                  base |> select(-any_of(c("id"))))

base$empty <- st_is_empty(base)
base <- base |> filter(!empty) |> select(-empty)

base <- base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()),.before=1)
base$area <- st_area(base)

base2 <- base |> select(-shape)
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)

base |>
  leaflet() |>
  addTiles() |>
  addPolygons(fillColor = "orange")


rm(base)




## NSW ----
state       <- "New South Wales"
state_short <- "Nsw"
ceds_2018 <- list_divisions(filters=list(StateAb="NSW",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2000:2599,2619:2899,2921:2999,2406,2540,2611,3585,3586,3644,3691,3707,4380,4377,4383,4385)

data_base <- load_aussiemaps_gpkg("2016_New.South.Wales")
data_base <- st_read(here("data-raw","2016_New South Wales.gpkg"))
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
              filter(STE_NAME_2016==state)

b <- b |> st_make_valid()
union <- st_union(data_base)
union <- union |> st_make_valid()
base <- st_difference(b,union)
base <- base |>
        rename("geom"="shape") |>
        st_make_valid()
base$empty <- st_is_empty(base)

base <- base |>
        filter(!empty) |>
        select(-empty)

base <- base |>
  st_collection_extract()


source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))

base <- base |> st_make_valid()
base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]

base <- bind_rows(data_base |> select(-any_of(c("id"))),
                  base |> select(-any_of(c("id"))))


st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


#rm(base)

## Victoria----
state       <- "Victoria"
state_short <- "Vic"
ceds_2018 <- list_divisions(filters=list(StateAb="VIC",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- 3000:3999

data_base <- load_aussiemaps_gpkg("2016_Victoria")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
  filter(STE_NAME_2016==state)

# data_base <- data_base |> filter(CED_NAME_2016 %in% c("Murray","Mallee","Gippsland"))
# b <- b |> filter(SA1_MAINCODE_2016 %in% unique(data_base$SA1_MAINCODE_2016))

source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))

base <- bind_rows(data_base |> select(-any_of(c("id"))),
                  base |> select(-any_of(c("id"))))
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


#rm(base)


## Tasmania----
state       <- "Tasmania"
state_short <- "Tas"

poas_state <- 7000:7999
ceds_2018 <- list_divisions(filters=list(StateAb="TAS",`2016`=TRUE)) %>% pull(DivisionNm)

data_base <- load_aussiemaps_gpkg("2016_Tasmania")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
  filter(STE_NAME_2016==state)


data_base <- data_base |> filter(CED_NAME_2016=="Lyons")
existingsa1 <- data_base |> st_drop_geometry() |> distinct(SA1_MAINCODE_2016) |> pull()
b <- b |> filter(SA1_MAINCODE_2016 %in% existingsa1)

source(here("data-raw","2016","find_missing.R"))
source(here("data-raw","2016","sequence_2016.R"))

base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- st_cast(base,"MULTIPOLYGON")

base <- bind_rows(data_base |> select(-any_of(c("id"))),
                  base |>  select(-any_of(c("id"))))



st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)


rm(base)




## ACT----
state       <- "Australian Capital Territory"
state_short <- "Act"
ceds_2018 <- list_divisions(filters=list(StateAb="ACT",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2600:2618,2900:2920,2620,2540)


data_base <- load_aussiemaps_gpkg("2016_Australian.Capital.Territory")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
  filter(STE_NAME_2016==state)

source(here("data-raw","2016","find_missing.R"))
if(!is.null(base)){
source(here("data-raw","2016","sequence_2016.R"))

base <- bind_rows(data_base |> select(-id),
                  base |> select(-id))
}else{
  base <- data_base
}
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")))

rm(base)



## Other Territories----
state       <- "Other Territories"
state_short <- "Other"
poas_state <- c(2540,2899,6798,6799,7151)

ceds_2016_out <- load_geo(nonabs,"commonwealth_electoral_division_2016") %>%
  filter(CED_NAME_2016 %in% c("Fenner","Bean","Lingiari")) %>%
  st_drop_geometry() %>%
  select(CED_CODE_2016,CED_NAME_2016) %>%
  left_join(tribble(~ SA3_NAME_2016,~CED_NAME_2016,
                    "Christmas Island","Lingiari",
                    "Norfolk Island","Bean",
                    "Jervis Bay","Fenner",
                    "Cocos (Keeling) Islands","Lingiari",
                    "Migratory - Offshore - Shipping (OT)","Migratory - Offshore - Shipping (OT)",
                    "No usual address (OT)","No usual address (OT)"
  ),by="CED_NAME_2016")



data_base <- load_aussiemaps_gpkg("2016_Other.Territories")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
  filter(STE_NAME_2016==state)

source(here("data-raw","2016","find_missing.R"))

if(!is.null(base)){
  source(here("data-raw","2016","sequence_2016.R"))

  base <- bind_rows(data_base |> select(-id),
                    base |> select(-id))
}else{
  base <- data_base
}

base <- base |>st_make_valid()

st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn = TRUE)


rm(base)

## Western Australia ----
state       <- "Western Australia"
state_short <- "Wa"
ceds_2018 <- list_divisions(filters=list(StateAb="WA",`2016`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(6000:6797,0872)

data_base <- load_aussiemaps_gpkg("2016_Western.Australia")
b         <-  load_geo(main, layer = "statistical_area_level_1_2016")  |>
  filter(STE_NAME_2016==state)

source(here("data-raw","2016","find_missing.R"))

if(!is.null(base)){
  source(here("data-raw","2016","sequence_2016.R"))

  base <- bind_rows(data_base |> select(-id),
                    base |> select(-id))
}else{
  base <- data_base
}
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")),append = FALSE,delete_dsn = TRUE)



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


files <- dir_ls(here("data-raw"),regexp = "gpkg")
structure <- NULL

for(file in files){

  data <- st_read(file) |>
    mutate(id=str_c(STE_CODE_2016,"-",row_number()))
  data$area <- st_area(data)

  st_write(data,file,append = FALSE,delete_dsn = TRUE)

  struct_i <- data |> st_drop_geometry()

  if(is.null(structure)){
    structure <- struct_i

  }else{
    structure <- bind_rows(structure,struct_i)
  }




}
geo_structure  <- structure
rm(data,struct_i,structure)

#save structure, create proportions ----

geo_structure <- geo_structure |>
  relocate(id,.before=1)

save_zip_parquet(geo_structure,"2016_structure",here("data-raw","processed"))

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

