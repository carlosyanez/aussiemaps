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


main        <- here("data-raw","source","asgs2011absstructures.gpkg")
nonabs      <- here("data-raw","source","asgs2011nonabsstructures.gpkg")
#nonabs2018  <- here("data-raw","source","asgs2011nonabsstructures.gpkg")
#indigenous  <- here("data-raw","source","asgs2011absstructuresindigenousstructure.gpkg")
#other       <- here("data-raw","source","asgs2011absstructuressignificanturbanareasurbancentresandlocalitiessectionofstate.gpkg")


main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
#indigenous_layers <- rgdal::ogrListLayers(indigenous)
#other_layers      <- rgdal::ogrListLayers(other)



files <- dir_ls(here("data-raw"),regexp = "gpkg")
structure <- NULL

for(file in files){

  data <- st_read(file) |>
          mutate(id=str_c(STE_CODE_2011,"-",row_number()))

  st_write(data,file,append = FALSE,delete_dsn = TRUE)

  struct_i <- data |> st_drop_geometry()

  if(is.null(structure)){
    structure <- struct_i

  }else{
    structure <- bind_rows(structure,struct_i)
  }




}
rm(data)


### find states to remediate-----

file <- files[1]


#structure <- data_base2 |> st_drop_geometry()
cols <- colnames(structure)
cols <- cols[str_detect(cols,"area|id",TRUE)]
cols <- cols[str_detect(cols,"STE",TRUE)]
missing <- tibble()

for(col_name in cols){
 missing_i <- structure |>
    select(any_of(col_name),STE_NAME_2011) |>
    rename("col"=col_name) |>
    count(col,STE_NAME_2011) |>
    filter(is.na(col))

  if(nrow(missing_i)>0){
    missing <- bind_rows(missing,
                         missing_i |> mutate(col=col_name))

  }

}

states_to_remediate <- missing |> distinct(STE_NAME_2011) |> pull()
states_to_remediate <- states_to_remediate[str_detect(states_to_remediate,"Other Territories",TRUE)]

geo_structure <- tibble()

## WA ------
state <- states_to_remediate[7]
missing |> filter(STE_NAME_2011==state)
#data_base <- load_aussiemaps_gpkg("2011_Western.Australia.gpkg")
data_base <- st_read(here("data-raw","2011_Western Australia.gpkg"))

data_base$empty <- st_is_empty(data_base)
data_base <- data_base |>
            filter(!empty) |>
            filter(str_detect(SA2_NAME_2011,"Offshore",TRUE)) |>
            select(-empty)

#CED ----

missing_ced <- data_base |> st_drop_geometry() |>
               filter(is.na(CED_CODE_2011)) |>
               select(id,SA1_MAINCODE_2011,CED_CODE_2011,CED_NAME_2011)

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(CED_CODE_2011)) |>
  filter(SA1_MAINCODE_2011 %in% missing_ced$SA1_MAINCODE_2011) |>
  distinct(SA1_MAINCODE_2011,CED_CODE_2011,CED_NAME_2011)

found <- found |>
  left_join(found |> count(SA1_MAINCODE_2011),by="SA1_MAINCODE_2011") |>
  rename("CED_CODE"="CED_CODE_2011",
         "CED_NAME"="CED_NAME_2011")

#1-to-1 correspondence, easy to fix

data_base <- data_base |>
  left_join(found |> select(-n),by="SA1_MAINCODE_2011") |>
  mutate(CED_NAME_2011=case_when(
    is.na(CED_NAME_2011) ~ CED_NAME,
    TRUE ~ CED_NAME_2011
  ),
  CED_CODE_2011=case_when(
    is.na(CED_CODE_2011) ~ CED_CODE,
    TRUE ~ CED_CODE_2011
  )) |>
  select(-CED_CODE,-CED_NAME)

data_base |> st_drop_geometry() |> count(CED_NAME_2011) |> filter(is.na(CED_NAME_2011))

# POA ----

missing_poa <- data_base |>
  filter(is.na(POA_CODE_2011 )) |>
  select(-POA_CODE_2011 ,POA_NAME_2011 )

poa <- load_geo(nonabs,"post_code_area_2011", state=state)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2011")
intersects <- intersections(missing_poa,poa, base_id="id",
                                                          bigger_id="POA_CODE_2011",
                                                          base_empty_label="id",
                                                          bigger_empty_label="POA_NAME_2011",
                                                          threshold = 0
)
intersects <- intersects |> mutate(POA_NAME_2011=POA_CODE_2011)

data_base <- data_base |>
  filter(!is.na(POA_CODE_2011)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base <- st_difference(data_base)

data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

data_base$area <- st_area(data_base)


## geo-structure, save -----
st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")),overwrite_dsn=TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())



## NT ------
state <- states_to_remediate[1]
missing |> filter(STE_NAME_2011==state)
#data_base <- load_aussiemaps_gpkg("2011_Northern.Territory.gpkg")
data_base <- st_read(here("data-raw","2011_Northern Territory.gpkg"))

# postcodes loaded incorreclty, need to re-do ----
data_base  <- data_base |> mutate(id=row_number())
poas_state <- str_pad(c(0800:0899,4825),4,"left","0")

ind <- load_geo(nonabs, layer="post_code_area_2011") %>%
  filter(POA_CODE_2011 %in% as.character(poas_state))

if(any(str_detect(colnames(ind),"shape"))) {ind <- ind |> rename("geom"="shape")}

data_base <- data_base |>select(-POA_CODE_2011,-POA_NAME_2011)

full_overlap <- full_coverage(data_base,
                              bigger=ind,
                              base_id="id",
                              bigger_id="POA_CODE_2011")

overlapped <- full_overlap %>%
  distinct(id) %>%
  mutate(dummy=TRUE)

base_renmant <- data_base  %>%
  left_join(overlapped,by="id") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

intersects <- intersections(base_renmant,
                            bigger=ind,
                            base_id="id",
                            bigger_id="POA_CODE_2011",
                            base_empty_label="SA2_NAME_2011",
                            bigger_empty_label="POA_NAME_2011"
)

intersected <- intersects %>%
  distinct(id) %>%
  mutate(dummy=TRUE)

non_matched <- base_renmant %>%
  left_join(intersected,by="id") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

data_base <- data_base |>
  select(-any_of(c("POA_CODE_2011","POA_NAME_2011"))) |>
  filter(!(id %in% intersected$id)) |>
  left_join(full_overlap |> mutate(POA_NAME_2011=POA_CODE_2011),by="id") |>
  bind_rows(intersects)

# SA4 -----

missing_sa4 <- data_base |> st_drop_geometry() |>
  filter(is.na(SA4_CODE_2011)) |>
  select(id,SA1_MAINCODE_2011,SA4_CODE_2011 ,SA4_NAME_2011 )

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(SA4_CODE_2011)) |>
  filter(SA1_MAINCODE_2011 %in% missing_sa4$SA1_MAINCODE_2011) |>
  distinct(SA1_MAINCODE_2011,SA4_CODE_2011 ,SA4_NAME_2011)

found <- found |>
  left_join(found |> count(SA1_MAINCODE_2011),by="SA1_MAINCODE_2011") |>
  rename("SA4_CODE"="SA4_CODE_2011",
         "SA4_NAME"="SA4_NAME_2011")

data_base <- data_base |>
  left_join(found |> select(-n),by="SA1_MAINCODE_2011") |>
  mutate(SA4_NAME_2011=case_when(
    is.na(SA4_NAME_2011) ~ SA4_NAME,
    TRUE ~ SA4_NAME_2011
  ),
  SA4_CODE_2011=case_when(
    is.na(SA4_CODE_2011) ~ SA4_CODE,
    TRUE ~ SA4_CODE_2011
  )) |>
  select(-SA4_CODE,-SA4_NAME)

data_base |> st_drop_geometry()  |> filter(is.na(SA4_NAME_2011))
#is migratory /offshore, remove
data_base <- data_base |> filter(str_detect(SA2_NAME_2011,"Offshore",TRUE))

## IARE -----

missing_IARE <- data_base |> st_drop_geometry() |>
  filter(is.na(IARE_CODE_2011)) |>
  select(id,SA1_MAINCODE_2011,IARE_CODE_2011 ,IARE_NAME_2011 )

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(IARE_CODE_2011)) |>
  filter(SA1_MAINCODE_2011 %in% missing_IARE$SA1_MAINCODE_2011) |>
  distinct(SA1_MAINCODE_2011,IARE_CODE_2011 ,IARE_NAME_2011)

###duplicated - will need to do via intersect metho
ind <- load_geo(main, layer="indigenous_area_2011") %>%
  filter(STE_NAME_2011==state) %>%
  select(-STE_CODE_2011,-STE_NAME_2011)

missing_IARE <- data_base |>
  filter(is.na(IARE_CODE_2011)) |>
  select(-IARE_CODE_2011 ,-IARE_NAME_2011)


intersects <- intersections(missing_IARE,
                            bigger=ind,
                            base_id="id",
                            bigger_id="IARE_CODE_2011",
                            base_empty_label="SA2_NAME_2011",
                            bigger_empty_label="IARE_NAME_2011"
)

data_base <- data_base |>
  filter(!is.na(IARE_CODE_2011)) |>
  bind_rows(intersects)

### TR----
###it was migratory zone
### CED
###it was migratory zone
## Year ----

data_base <- data_base |> mutate(Year=2011)




### save ----
data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

data_base$area <- st_area(data_base)


st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## NSW ------
state <- states_to_remediate[2]
missing |> filter(STE_NAME_2011==state)
data_base <- st_read(here("data-raw","2011_New South Wales.gpkg"))

data_base <- data_base |>
             filter(str_detect(SA2_NAME_2011,"Migratory",TRUE)) |>
             mutate(Year=2011)

#TR ----
data_base |>
  filter(is.na(TR_CODE_2011))
#it was migratory zone

#CED ---
data_base |>
  filter(is.na(CED_CODE_2011))
#it was migratory zone

# POA ----

data_base |>
  filter(is.na(POA_NAME_2011))
# just for, easy fix

data_base <- data_base |>
  mutate(POA_CODE_2011=case_when(
    SSC_NAME_2011 =="Nanima" ~ "2582",
    SSC_NAME_2011 =="Springrange" ~ "2618",
    SSC_NAME_2011 =="Wallaroo (NSW)" ~ "2618",
    TRUE ~ POA_CODE_2011
  ),
  POA_NAME_2011=POA_CODE_2011)

# write ----

data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

data_base$area <- st_area(data_base)

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")),delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## QLD ------
state <- states_to_remediate[3]
missing |> filter(STE_NAME_2011==state)
data_base <- st_read(here("data-raw","2011_Queensland.gpkg"))

#all good with QLD

data_base <- st_make_valid(data_base)
data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

data_base$area <- st_area(data_base)

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")),delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure |> mutate(area=as.numeric(area)),
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))


## SA ------
state <- states_to_remediate[4]
missing |> filter(STE_NAME_2011==state)
 #data_base <- load_aussiemaps_gpkg("2011_South.Australia.gpkg")
data_base <- st_read(here("data-raw","2011_South Australia.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2011,"Migratory",TRUE)) |>
  mutate(Year=2011)

data_base |>
  filter(is.na(TR_CODE_2011))
data_base |>
  filter(is.na(CED_CODE_2011))

missing_poa <- data_base |>
  filter(is.na(POA_CODE_2011)) |>
  select(-POA_CODE_2011,-POA_CODE_2011)

poa <- load_geo(nonabs,"post_code_area_2011", state=state)


#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2011")
intersects <- intersections(missing_poa,poa, base_id="id",
                            bigger_id="POA_CODE_2011",
                            base_empty_label="id",
                            bigger_empty_label="POA_NAME_2011",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(POA_CODE_2011)) |>
  bind_rows(intersects) |>
  mutate(POA_NAME_2011=POA_CODE_2011)|>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

## VIC ------
state <- states_to_remediate[6]

data_base <- st_read(here("data-raw","2011_Victoria.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2011,"Migratory",TRUE)) |>
  mutate(Year=2011)

missing |> filter(STE_NAME_2011==state)


data_base |>
  filter(is.na(TR_CODE_2011))

data_base |>
  filter(is.na(CED_CODE_2011))

data_base |>
  filter(is.na(POA_CODE_2011))

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))


### TAS -----

state <- states_to_remediate[5]

data_base <- st_read(here("data-raw","2011_Tasmania.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2011,"Migratory",TRUE)) |>
  mutate(Year=2011)

missing |> filter(STE_NAME_2011==state)

data_base |>
  filter(is.na(TR_CODE_2011))

data_base |>
  filter(is.na(CED_CODE_2011))

data_base |>
  filter(is.na(POA_CODE_2011))

#POA cannot be fixed manually

missing_poas <-
  data_base |>
  filter(is.na(POA_CODE_2011)) |>
  select(-POA_CODE_2011,-POA_NAME_2011)

poa <- load_geo(nonabs,"post_code_area_2011", state=state)


#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2011")
intersects <- intersections(missing_poa,poa, base_id="id",
                            bigger_id="POA_CODE_2011",
                            base_empty_label="id",
                            bigger_empty_label="POA_NAME_2011",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(POA_CODE_2011)) |>
  bind_rows(intersects) |>
  mutate(POA_NAME_2011=POA_CODE_2011)|>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

# Other Territories

state <- "Other Territories"

data_base <- st_read(here("data-raw","2011_Other Territories.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2011,"Migratory",TRUE)) |>
  mutate(Year=2011) |>
  mutatE(POA_NAME_2011=POA_CODE_2011)

missing |> filter(STE_NAME_2011==state)

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2011,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2011_",state,".gpkg")), delete_dsn = TRUE)


## add remainder states to geo structure, calculate proportions -----
geo_structure |> distinct(STE_NAME_2011) |>filter(!(STE_NAME_2011 %in% states_to_remediate))

geo_structure <- bind_rows(st_read(here("data-raw",str_c("2011_New South Wales.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Victoria.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Queensland.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Western Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_South Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Northern Territory.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Australian Capital Territory.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Other Territories.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2011_Tasmania.gpkg"))) |> st_drop_geometry())

save_zip_parquet(geo_structure,"2011_structure",here("data-raw","processed"))

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
