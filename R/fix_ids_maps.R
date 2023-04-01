library(aussiemaps)
library(tidyverse)
library(here)
library(fs)
library(sf)

source(here("R","internal.R"))
source(here("R","cache_management.R"))
source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))


files <- dir_ls(here("data-raw"),regexp = "gpkg$")

postcodes <- read_csv("https://www.matthewproctor.com/Content/postcodes/australian_postcodes.csv") |>
  mutate(locality=str_to_title(locality),
         locality=str_squish(locality))

postcodes <- postcodes |>
  filter(str_detect(postcode,"^02\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^1\\d{3}$",TRUE)) |>
  filter(str_detect(postcode,"^8\\d{3}$",TRUE)) |>
  filter(str_detect(postcode,"^1\\d{3}$",TRUE)) |>
  filter(str_detect(postcode,"^9\\d{3}$",TRUE)) |>
  filter(str_detect(postcode,"^58\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^59\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^68\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^69\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^78\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^79\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^09\\d{2}$",TRUE)) |>
  filter(str_detect(postcode,"^2001$",TRUE)) |>
  filter(str_detect(postcode,"^2601$",TRUE)) |>
  filter(str_detect(postcode,"^3001$",TRUE)) |>
  filter(str_detect(postcode,"^4001$",TRUE)) |>
  filter(str_detect(postcode,"^5001$",TRUE)) |>
  filter(str_detect(postcode,"^6001$",TRUE)) |>
  filter(str_detect(postcode,"^7001$",TRUE)) |>
  filter(str_detect(postcode,"^0801$",TRUE)) |>
  filter(str_detect(postcode,"^2001$",TRUE)) |>
  filter(!(type %in% c("Post Office Boxes","LVR")))     |>
  filter(str_detect(status,"Removed",TRUE)) |>
  distinct(locality,state,postcode)

multiple_postcodes <-
  postcodes |> count(locality,state) |> filter(n>1)

postcodes <- postcodes |>
  left_join(multiple_postcodes,by=c("locality","state")) |>
  filter(is.na(n)) |>
  select(-n)

dupes <- postcodes |> count(locality,state,sa4name)  |> filter(n>1)

dupes <- postcodes |> left_join(dupes, by=c("locality","state","sa4name")) |> filter(!is.na(n))

geo_structure <- tibble()

## GO MANUALLY
## ACT ----
map_file <- files[1]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)

map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))

missing_POAS <- map |>
                st_drop_geometry() |>
                filter(is.na(POA_CODE_2021))
#NO missing POAS, fix id, copy structure, save

map <- map |>
        mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
        relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)

## NSW ----
map_file <- files[2]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))

missing_POAS |>
  left_join(postcodes  |> filter(state=="NSW") |> select(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))
#only partially found, doing it manually

map <- map |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & SAL_NAME_2021=="Springrange" ~ "2618",
                  is.na(.x) & SAL_NAME_2021=="Wallaroo" ~ "2618",
                  TRUE ~ .x
                )))


map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)

## NT ----
map_file <- files[3]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))

missing_POAS |>
  left_join(postcodes  |> filter(state=="NT") |> distinct(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))
#only partially found, doing it manually

#first pass,
map <- map |>
  left_join(postcodes  |> filter(state=="NT") |> distinct(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))  |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & !is.na(postcode) ~ postcode,
                  TRUE ~ .x
                ))) |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & SAL_NAME_2021=="Parap" ~ "0820",
                  is.na(.x) & SAL_NAME_2021=="Nightcliff" ~ "0810",
                  is.na(.x) & SAL_NAME_2021=="McMinns Lagoon" ~ "0822",
                  is.na(.x) & SAL_NAME_2021=="Virginia" ~ "0834",
                  is.na(.x) & SAL_NAME_2021=="Palmerston City" ~ "0830",
                  is.na(.x) & SAL_NAME_2021=="Amoonguna" ~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Tennant Creek" ~ "0860",
                  is.na(.x) & SAL_NAME_2021=="Nhulunbuy" ~ "0880",
                  is.na(.x) & SAL_NAME_2021=="Katherine" ~ "0850",
                  is.na(.x) & SAL_NAME_2021=="Casuarina" ~ "0810",
                  is.na(.x) & SAL_NAME_2021=="Alice Springs" ~ "0870",
                  is.na(.x) & SAL_NAME_2021=="Winnellie" ~ "0820",
                  is.na(.x) & SAL_NAME_2021=="Numbulwar" ~ "0852",
                  is.na(.x) & SAL_NAME_2021=="Coolalinga" ~ "0839",
                  is.na(.x) & SAL_NAME_2021=="Charles Darwin" ~ "0820",
                  is.na(.x) & SAL_NAME_2021=="McArthur" ~ "0852",
                  TRUE ~ .x
                ))) |>
  select(-postcode)


map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)


## Other Territories ----
map_file <- files[4]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))

missing_POAS |>
  left_join(postcodes  |> filter(state=="NT") |> distinct(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))

#no missing poas

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)


## Queensland ----
map_file <- files[5]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))
#no missing poas

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)



## South Australia ----
map_file <- files[6]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))
#no missing poas

map <- map |>
  left_join(postcodes  |> filter(state=="NT") |> distinct(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))  |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & !is.na(postcode) ~ postcode,
                  TRUE ~ .x
                ))) |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & SAL_NAME_2021=="Kalka"~ "5710",
                  is.na(.x) & SAL_NAME_2021=="Pukatja"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Kaltjiti"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Adelaide Airport"~ "5950",
                  is.na(.x) & SAL_NAME_2021=="Watarru"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Pipalyatjara"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Mimili"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Iwantja"~ "0872",
                  TRUE ~ .x
                ))) |>
  select(-postcode)
  #filter(is.na(POA_CODE_2021)) #|>
  #distinct(SAL_NAME_2021) |>
  #clipr::write_clip()

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)



## Tasmania ----
map_file <- files[7]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))
#no missing poas

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)



## Victoria ----
map_file <- files[8]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))
#no missing poas

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)




## WA ----
map_file <- files[9]

map <- st_read(map_file)
#remove empty
map$empty <- st_is_empty(map)
map <- map |> filter(!empty) |> select(-empty)
map <- map |> mutate(across(where(is.character), ~ str_squish(.x)))


missing_POAS <- map |>
  st_drop_geometry() |>
  filter(is.na(POA_CODE_2021))

map <- map |>
  left_join(postcodes  |> filter(state=="NT") |> distinct(locality,postcode),
            by=c("SAL_NAME_2021"="locality"))  |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & !is.na(postcode) ~ postcode,
                  TRUE ~ .x
                ))) |>
  mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                ~ case_when(
                  is.na(.x) & SAL_NAME_2021=="Gibson Desert North"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Papulankutja"~ "6642",
                  is.na(.x) & SAL_NAME_2021=="Warakurna"~ "6642",
                  is.na(.x) & SAL_NAME_2021=="Irrunytju"~ "0872",
                  is.na(.x) & SAL_NAME_2021=="Mantamaru"~ "6642",
                  TRUE ~ .x
                ))) |>
 select(-postcode)
#filter(is.na(POA_CODE_2021)) # |>
#distinct(SAL_NAME_2021) |>
#clipr::write_clip()

map <- map |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  relocate(id,.before=1)

st_write(map,map_file,delete_dsn=TRUE)
save_zip_gpkg(map_file,here("data-raw"),here("data-raw","processed"))

map$area <- st_area(map)
struct_i <- map |> st_drop_geometry()
geo_structure <- bind_rows(struct_i,geo_structure)

geo_structure <- distinct(geo_structure)

# save structure
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



