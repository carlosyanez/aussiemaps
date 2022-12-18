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

keep_vars <- c(ls(),"keep_vars")
# Tasmania ----
state       <- "Tasmania"
state_short <- "Tas"

poas_state <- 7000:7999
ceds_2018 <- list_divisions(filters=list(StateAb="ACT",`2019`=TRUE)) %>% pull(DivisionNm)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# ACT ----
state       <- "Australian Capital Territory"
state_short <- "Act"
ceds_2018 <- list_divisions(filters=list(StateAb="ACT",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2600:2618,2900:2920,2620,2540)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Victoria ----
state       <- "Victoria"
state_short <- "Vic"
ceds_2018 <- list_divisions(filters=list(StateAb="VIC",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- 3000:3009

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# NSW ----
state       <- "New South Wales"
state_short <- "Nsw"
ceds_2018 <- list_divisions(filters=list(StateAb="NSW",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(2000:2599,2619:2899,2921:2999,2406,2540,2611,3585,3586,3644,3691,3707,4380,4377,4383,4385)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])


# Queensland ----
state       <- "Queensland"
state_short <- "Qld"
ceds_2018 <- list_divisions(filters=list(StateAb="QLD",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(4000:4999,2406)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# South Australia ----
state       <- "South Australia"
state_short <- "Sa"
ceds_2018 <- list_divisions(filters=list(StateAb="SA",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(5000:5799,0872)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Western Australia ----
state       <- "Western Australia"
state_short <- "Wa"
ceds_2018 <- list_divisions(filters=list(StateAb="WA",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(6000:6797,0872)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Northern Territory ----
state       <- "Northern Territory"
state_short <- "Nt"
ceds_2018 <- list_divisions(filters=list(StateAb="NT",`2019`=TRUE)) %>% pull(DivisionNm)
poas_state <- c(0800:0899,4825)

source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])

# Other Territories ----
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


source(here("data-raw","sequence.R"))
rm(list=ls()[!(ls() %in% keep_vars)])



