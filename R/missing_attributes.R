library(tidyverse)
library(sf)
library(fs)
library(arrow)
library(leaflet)
library(aussiemaps)
library(sfarrow)
library(auspol)
library(here)

source(here("data-raw","aux_save.R"))
source(here("data-raw","functions.R"))
source(here("R","internal.R"))


main        <- here("data-raw","source","ASGS_2021_Main_Structure_GDA2020.gpkg")
nonabs      <- here("data-raw","source","ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2022.gpkg")
indigenous  <- here("data-raw","source","ASGS_Ed3_2021_Indigenous_Structure_GDA2020.gpkg")
other       <- here("data-raw","source","ASGS_2021_SUA_UCL_SOS_SOSR_GPKG_GDA2020.gpkg")

main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
indigenous_layers <- rgdal::ogrListLayers(indigenous)
other_layers      <- rgdal::ogrListLayers(other)


structure <- list_structure(2021)

cols <- colnames(structure)
cols <- cols[str_detect(cols,"area|id",TRUE)]
cols <- cols[str_detect(cols,"STATE",TRUE)]
missing <- tibble()

for(col_name in cols){
 missing_i <- structure |>
    select(any_of(col_name),STATE_NAME_2021) |>
    rename("col"=col_name) |>
    count(col,STATE_NAME_2021) |>
    filter(is.na(col))

  if(nrow(missing_i)>0){
    missing <- bind_rows(missing,
                         missing_i |> mutate(col=col_name))

  }

}

states_to_remediate <- missing |> distinct(STATE_NAME_2021) |> pull()
states_to_remediate <- states_to_remediate[str_detect(states_to_remediate,"Other Territories",TRUE)]

geo_structure <- tibble()

## WA ------
state <- states_to_remediate[1]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_Western.Australia.gpkg")

#CED ----

missing_ced <- data_base |> st_drop_geometry() |>
               filter(is.na(CED_CODE_2021)) |>
               select(id,SA1_CODE_2021,CED_CODE_2021,CED_NAME_2021)

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(CED_CODE_2021)) |>
  filter(SA1_CODE_2021 %in% missing_ced$SA1_CODE_2021) |>
  distinct(SA1_CODE_2021,CED_CODE_2021,CED_NAME_2021)

found <- found |>
  left_join(found |> count(SA1_CODE_2021),by="SA1_CODE_2021") |>
  rename("CED_CODE"="CED_CODE_2021",
         "CED_NAME"="CED_NAME_2021")

#1-to-1 correspondence, easy to fix

data_base <- data_base |>
  left_join(found |> select(-n),by="SA1_CODE_2021") |>
  mutate(CED_NAME_2021=case_when(
    is.na(CED_NAME_2021) ~ CED_NAME,
    TRUE ~ CED_NAME_2021
  ),
  CED_CODE_2021=case_when(
    is.na(CED_CODE_2021) ~ CED_CODE,
    TRUE ~ CED_CODE_2021
  )) |>
  select(-CED_CODE,-CED_NAME)

data_base |> st_drop_geometry |> count(CED_NAME_2021) |> filter(is.na(CED_NAME_2021))

# SAL ----

missing_sal <- data_base |>
  filter(is.na(SAL_CODE_2021)) |>
  select(id)

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
    mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(SAL_CODE_2021)) |>
  filter(SA1_CODE_2021 %in% missing_sal$SA1_CODE_2021) |>
  distinct(SA1_CODE_2021,SAL_CODE_2021,SAL_NAME_2021)

found <- found |>
  left_join(found |> count(SA1_CODE_2021),by="SA1_CODE_2021") |>
  rename("SAL_CODE"="SAL_CODE_2021",
         "SAL_NAME"="SAL_NAME_2021")

#not unique, requires, one-to-one mapping
for(i in 1:nrow(missing_sal)){
message(glue::glue("{i} out of {nrow(missing_sal)}"))
missing_i <- data_base |> filter(id==missing_sal[i,]$id)
possible_i <- data_base |>
                filter(SA1_CODE_2021==missing_sal[i,]$SA1_CODE_2021) |>
                filter(id!=missing_sal[i,]$id)

touches <- st_touches(missing_i,possible_i,FALSE)
possible_i <- possible_i[touches,]

if(nrow(possible_i)>1){
  possible_i <- possible_i |> filter(area==max(area))
}
missing_sal[i,]$SAL_CODE_2021 <- possible_i[1,]$SAL_CODE_2021
missing_sal[i,]$SAL_NAME_2021 <- possible_i[1,]$SAL_NAME_2021
}
