# setup, aux ----
library(here)
library(fs)
library(sf)
library(tidyverse)
library(arrow)
library(sfarrow)
library(progressr)
library(auspol)


handlers(global = TRUE)

source(here("data-raw","functions.R"))

#from https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files

#location, layers ----

main        <- here("data-raw","source","asgs2011absstructures.gpkg")
nonabs      <- here("data-raw","source","asgs2011nonabsstructures.gpkg")
#nonabs2018  <- here("data-raw","source","asgs2011nonabsstructures.gpkg")
#indigenous  <- here("data-raw","source","asgs2011absstructuresindigenousstructure.gpkg")
#other       <- here("data-raw","source","asgs2011absstructuressignificanturbanareasurbancentresandlocalitiessectionofstate.gpkg")


main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
#indigenous_layers <- rgdal::ogrListLayers(indigenous)
#other_layers      <- rgdal::ogrListLayers(other)


temp_file <- here("data-raw","temp_df.parquet")
base_file <- here("data-raw","base_sf3.parquet")
#ced_file <- here("data-raw","source","divisions-Aug-2021-by-2011-SA1")

keep_vars <- c(ls(),"keep_vars")
# Tasmania ----
state       <- "Tasmania"
state_short <- "Tas"

poas_state <- 7000:7999

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# ACT ----
state       <- "Australian Capital Territory"
state_short <- "Act"
poas_state <- c(2600:2618,2900:2920,2620,2540)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Victoria ----
state       <- "Victoria"
state_short <- "Vic"
ceds_2018 <- list_divisions(filters=list(StateAb="VIC",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- 3000:3999

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# NSW ----
state       <- "New South Wales"
state_short <- "Nsw"
ceds_2018 <- list_divisions(filters=list(StateAb="NSW",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2000:2599,2619:2899,2921:2999,2406,2540,2611,3585,3586,3644,3691,3707,4380,4377,4383,4385)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])


# Queensland ----
state       <- "Queensland"
state_short <- "Qld"
ceds_2018 <- list_divisions(filters=list(StateAb="QLD",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(4000:4999,2406)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# South Australia ----
state       <- "South Australia"
state_short <- "Sa"
ceds_2018 <- list_divisions(filters=list(StateAb="SA",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(5000:5799,0872)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Western Australia ----
state       <- "Western Australia"
state_short <- "Wa"
ceds_2018 <- list_divisions(filters=list(StateAb="WA",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(6000:6797,0872)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Northern Territory ----
state       <- "Northern Territory"
state_short <- "Nt"
ceds_2018 <- list_divisions(filters=list(StateAb="NT",`2010`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(0800:0899,4825)

source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Other Territories ----
state       <- "Other Territories"
state_short <- "Other"
poas_state <- c(2540,2899,6798,6799,7151)

ceds_2011_out <- load_geo(nonabs,"commonwealth_electoral_division_2011") %>%
              filter(CED_NAME_2011 %in% c("Fraser","Fenner","Lingiari")) %>%
              st_drop_geometry() %>%
              select(CED_CODE_2011,CED_NAME_2011) %>%
              left_join(tribble(~ SA3_NAME_2011,~CED_NAME_2011,
                                "Christmas Island","Lingiari",
                                "Norfolk Island","Fraser",
                                "Jervis Bay","Fraser",
                                "Cocos (Keeling) Islands","Lingiari",
                                "Migratory - Offshore - Shipping (OT)","Migratory - Offshore - Shipping (OT)",
                                "No usual address (OT)","No usual address (OT)"
              ),by="CED_NAME_2011")


source(here("data-raw","2011","sequence_2011.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# create master list, with areas ----

dir_create(here("data-raw","processed"))

rm(list=ls())
source(here("data-raw","aux_save.R"))
files <- dir_ls(here("data-raw"),regexp = "2011.*gpkg$")

# files <- dir_ls(aussiemaps::find_maps_cache(),regexp = "gpkg")
#  files <- files[str_detect(files,"\\/2011")]
#  files <- files[str_detect(files,"[0-9]\\.gpkg",TRUE)]

geo_structure <- tibble()

for(file in files){
  shapes <- st_read(file)
  shapes$empty <- st_is_empty(shapes)
  shapes <- shapes |> filter(!empty) |> select(-empty)

  shapes <- shapes |>
    st_make_valid() |>
    st_cast("MULTIPOLYGON")

  shapes$area <- st_area(shapes)

  #if(str_detect(file,"Tasmania")){
  #  shapes <- shapes |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))
  #  st_write(shapes,file,delete_dsn=TRUE)
  #}


  shapes <- shapes %>% st_drop_geometry()
  geo_structure <- bind_rows(geo_structure,shapes)



}

save_zip_parquet(geo_structure,"2011_structure",here("data-raw","processed"))

geo_cols <- colnames(geo_structure)
geo_cols <- geo_cols[str_detect(geo_cols,"CODE")]


attributes <- geo_structure[1,] %>%
  select(-any_of(c("area","Year"))) %>%
  pivot_longer(-id,names_to="attributes",values_to = "value") %>%
  select(attributes) %>%
  mutate(Year=2011)

save_zip_parquet(attributes,"2011_attributes",here("data-raw","processed"))

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

for(file in files){
  save_zip_gpkg(file,here("data-raw"),here("data-raw","processed"))
}




