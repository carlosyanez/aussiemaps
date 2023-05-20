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


main        <- here("data-raw","source","asgs2016absstructuresmainstructureandgccsa.gpkg")
nonabs      <- here("data-raw","source","asgs2016nonabsstructures.gpkg")
indigenous  <- here("data-raw","source","asgs2016absstructuresindigenousstructure.gpkg")
other       <- here("data-raw","source","asgs2016absstructuressignificanturbanareasurbancentresandlocalitiessectionofstate.gpkg")

main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)
indigenous_layers <- rgdal::ogrListLayers(indigenous)
other_layers      <- rgdal::ogrListLayers(other)



files <- dir_ls(here("data-raw"),regexp = "gpkg")
structure <- NULL

for(file in files){

  data <- st_read(file) |>
          mutate(id=str_c(STE_CODE_2016,"-",row_number()))

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
cols <- cols[str_detect(cols,"[Yy]ear",TRUE)]
cols <- cols[str_detect(cols,"STE",TRUE)]
missing <- tibble()

for(col_name in cols){
 missing_i <- structure |>
    select(any_of(col_name),STE_NAME_2016) |>
    rename("col"=col_name) |>
    replace_na(list(col="NA"))  |>
    mutate(col=as.character(col)) |>
    count(col,STE_NAME_2016) |>
    filter(col=="NA")  |>
    mutate(col=col_name)

  if(nrow(missing_i)>0){
    missing <- bind_rows(missing,
                         missing_i)

  }

}

states_to_remediate <- missing |> distinct(STE_NAME_2016) |> pull()
states_to_remediate <- states_to_remediate[str_detect(states_to_remediate,"Other Territories",TRUE)]

geo_structure <- tibble()

## WA ------
#state <- states_to_remediate[7]
state <- states_to_remediate[2]
missing |> filter(STE_NAME_2016==state)
#data_base <- load_aussiemaps_gpkg("2016_Western.Australia.gpkg")
data_base <- st_read(here("data-raw","2016_Western Australia.gpkg"))

data_base$empty <- st_is_empty(data_base)
data_base <- data_base |>
            filter(!empty) |>
            filter(str_detect(SA2_NAME_2016,"Offshore",TRUE)) |>
            select(-empty)

#CED ----

missing_ced <- data_base |> st_drop_geometry() |>
               filter(is.na(CED_CODE_2016)) |>
               select(id,SA1_MAINCODE_2016,CED_CODE_2016,CED_NAME_2016)

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(CED_CODE_2016)) |>
  filter(SA1_MAINCODE_2016 %in% missing_ced$SA1_MAINCODE_2016) |>
  distinct(SA1_MAINCODE_2016,CED_CODE_2016,CED_NAME_2016)

found <- found |>
  left_join(found |> count(SA1_MAINCODE_2016),by="SA1_MAINCODE_2016") |>
  rename("CED_CODE"="CED_CODE_2016",
         "CED_NAME"="CED_NAME_2016")

#1-to-1 correspondence, easy to fix

data_base <- data_base |>
  left_join(found |> select(-n),by="SA1_MAINCODE_2016") |>
  mutate(CED_NAME_2016=case_when(
    is.na(CED_NAME_2016) ~ CED_NAME,
    TRUE ~ CED_NAME_2016
  ),
  CED_CODE_2016=case_when(
    is.na(CED_CODE_2016) ~ CED_CODE,
    TRUE ~ CED_CODE_2016
  )) |>
  select(-CED_CODE,-CED_NAME)

data_base |> st_drop_geometry() |> count(CED_NAME_2016) |> filter(is.na(CED_NAME_2016))

# POA ----

missing_poa <- data_base |>
  filter(is.na(POA_CODE_2016 )) |>
  select(-POA_CODE_2016 ,POA_NAME_2016 )

poa <- load_geo(nonabs,"post_code_area_2016", state=state)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2016")
intersects <- intersections(missing_poa,poa, base_id="id",
                                                          bigger_id="POA_CODE_2016",
                                                          base_empty_label="id",
                                                          bigger_empty_label="POA_NAME_2016",
                                                          threshold = 0
)
intersects <- intersects |> mutate(POA_NAME_2016=POA_CODE_2016)

data_base <- data_base |>
  filter(!is.na(POA_CODE_2016)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base <- st_difference(data_base)

data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

data_base$area <- st_area(data_base)


## geo-structure, save -----
st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn=TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())



## NT ------
state <- states_to_remediate[4]
missing |> filter(STE_NAME_2016==state)
#data_base <- load_aussiemaps_gpkg("2016_Northern.Territory.gpkg")
data_base <- st_read(here("data-raw","2016_Northern.Territory.gpkg"))

# postcodes loaded incorreclty, need to re-do ----
data_base  <- data_base |> mutate(id=row_number())
poas_state <- str_pad(c(0800:0899,4825),4,"left","0")

ind <- load_geo(nonabs, layer="post_code_area_2016") %>%
  filter(POA_CODE_2016 %in% as.character(poas_state))

if(any(str_detect(colnames(ind),"shape"))) {ind <- ind |> rename("geom"="shape")}

data_base <- data_base |>select(-POA_CODE_2016,-POA_NAME_2016)

full_overlap <- full_coverage(data_base,
                              bigger=ind,
                              base_id="id",
                              bigger_id="POA_CODE_2016")

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
                            bigger_id="POA_CODE_2016",
                            base_empty_label="SA2_NAME_2016",
                            bigger_empty_label="POA_NAME_2016"
)

intersected <- intersects %>%
  distinct(id) %>%
  mutate(dummy=TRUE)

non_matched <- base_renmant %>%
  left_join(intersected,by="id") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

data_base <- data_base |>
  select(-any_of(c("POA_CODE_2016","POA_NAME_2016"))) |>
  filter(!(id %in% intersected$id)) |>
  left_join(full_overlap |> mutate(POA_NAME_2016=POA_CODE_2016),by="id") |>
  bind_rows(intersects)

# SA4 -----

missing_sa4 <- data_base |> st_drop_geometry() |>
  filter(is.na(SA4_CODE_2016)) |>
  select(id,SA1_MAINCODE_2016,SA4_CODE_2016 ,SA4_NAME_2016 )

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(SA4_CODE_2016)) |>
  filter(SA1_MAINCODE_2016 %in% missing_sa4$SA1_MAINCODE_2016) |>
  distinct(SA1_MAINCODE_2016,SA4_CODE_2016 ,SA4_NAME_2016)

found <- found |>
  left_join(found |> count(SA1_MAINCODE_2016),by="SA1_MAINCODE_2016") |>
  rename("SA4_CODE"="SA4_CODE_2016",
         "SA4_NAME"="SA4_NAME_2016")

data_base <- data_base |>
  left_join(found |> select(-n),by="SA1_MAINCODE_2016") |>
  mutate(SA4_NAME_2016=case_when(
    is.na(SA4_NAME_2016) ~ SA4_NAME,
    TRUE ~ SA4_NAME_2016
  ),
  SA4_CODE_2016=case_when(
    is.na(SA4_CODE_2016) ~ SA4_CODE,
    TRUE ~ SA4_CODE_2016
  )) |>
  select(-SA4_CODE,-SA4_NAME)

data_base |> st_drop_geometry()  |> filter(is.na(SA4_NAME_2016))
#is migratory /offshore, remove
data_base <- data_base |> filter(str_detect(SA2_NAME_2016,"Offshore",TRUE))

## IARE -----

missing_IARE <- data_base |> st_drop_geometry() |>
  filter(is.na(IARE_CODE_2016)) |>
  select(id,SA1_MAINCODE_2016,IARE_CODE_2016 ,IARE_NAME_2016 )

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(IARE_CODE_2016)) |>
  filter(SA1_MAINCODE_2016 %in% missing_IARE$SA1_MAINCODE_2016) |>
  distinct(SA1_MAINCODE_2016,IARE_CODE_2016 ,IARE_NAME_2016)

###duplicated - will need to do via intersect metho
ind <- load_geo(main, layer="indigenous_area_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

missing_IARE <- data_base |>
  filter(is.na(IARE_CODE_2016)) |>
  select(-IARE_CODE_2016 ,-IARE_NAME_2016)


intersects <- intersections(missing_IARE,
                            bigger=ind,
                            base_id="id",
                            bigger_id="IARE_CODE_2016",
                            base_empty_label="SA2_NAME_2016",
                            bigger_empty_label="IARE_NAME_2016"
)

data_base <- data_base |>
  filter(!is.na(IARE_CODE_2016)) |>
  bind_rows(intersects)

### TR----
###it was migratory zone
### CED
###it was migratory zone
## Year ----

data_base <- data_base |> mutate(Year=2016)




### save ----
data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

data_base$area <- st_area(data_base)


st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## NSW ------
state <- states_to_remediate[3]
missing |> filter(STE_NAME_2016==state)
data_base <- st_read(here("data-raw","2016_New South Wales.gpkg"))

data_base <- data_base |>
             filter(str_detect(SA2_NAME_2016,"Migratory",TRUE)) |>
             mutate(Year=2016)

#TR ----
data_base |>
  filter(is.na(TR_CODE_2016))
#it was migratory zone

#CED ---
data_base |>
  filter(is.na(CED_CODE_2016))
#it was migratory zone

# POA ----

data_base |>
  filter(is.na(POA_NAME_2016))
# just for, easy fix

data_base <- data_base |>
  mutate(POA_CODE_2016=case_when(
    SSC_NAME_2016 =="Nanima" ~ "2582",
    SSC_NAME_2016 =="Springrange" ~ "2618",
    SSC_NAME_2016 =="Wallaroo (NSW)" ~ "2618",
    SSC_NAME_2016 =="Cottonvale (NSW)" ~ "4375",
    TRUE ~ POA_CODE_2016
  ),
  POA_NAME_2016=POA_CODE_2016)

# write ----

data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

data_base$area <- st_area(data_base)

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## QLD ------
state <- states_to_remediate[1]
missing |> filter(STE_NAME_2016==state)
data_base <- st_read(here("data-raw","2016_Queensland.gpkg"))

##ILOC

missing_iloc <- data_base |>
  filter(is.na(ILOC_CODE_2016 )) |>
  select(-ILOC_CODE_2016 ,-ILOC_NAME_2016 ) |>
  mutate(id=row_number())

iloc <- load_geo(indigenous,"indigenous_location_2016", state=state)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2016")
intersects <- intersections(missing_iloc,iloc, base_id="id",
                            bigger_id="ILOC_CODE_2016",
                            base_empty_label="id",
                            bigger_empty_label="ILOC_NAME_2016",
                            threshold = 0
)
#intersects <- intersects |> mutate(ILOC_NAME_2016=ILOC_CODE_2016)

data_base <- data_base |>
  select(-id)   |>
  filter(!is.na(ILOC_CODE_2016)) |>
  bind_rows(intersects |> select(-id)) |>
  st_make_valid()

#all good with QLD

data_base <- st_make_valid(data_base)
data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

data_base$area <- st_area(data_base)

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")),delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure |> mutate(area=as.numeric(area)),
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))


## SA ------
state <- states_to_remediate[5]
missing |> filter(STE_NAME_2016==state)
 #data_base <- load_aussiemaps_gpkg("2016_South.Australia.gpkg")
data_base <- st_read(here("data-raw","2016_South Australia.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2016,"Migratory",TRUE)) |>
  mutate(Year=2016)

data_base |>
  filter(is.na(SSC_CODE_2016 ))
data_base |>
  filter(is.na(CED_CODE_2016))

# SSC -----

ind <- load_geo(nonabs, layer="state_suburb_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

missing_SSC <- data_base |>
  filter(is.na(SSC_CODE_2016)) |>
  select(-SSC_CODE_2016 ,-SSC_NAME_2016) |>
  mutate(id=row_number())


intersects <- intersections(missing_SSC,
                            bigger=ind,
                            base_id="id",
                            bigger_id="SSC_CODE_2016",
                            base_empty_label="SA2_NAME_2016",
                            bigger_empty_label="SSC_NAME_2016"
)

data_base <- data_base |>
  select(-id)          |>
  filter(!is.na(SSC_CODE_2016)) |>
  bind_rows(intersects |> select(-id))


## IARE -----

missing_ssc <- data_base |> st_drop_geometry() |>
  filter(is.na(SSC_CODE_2016)) |>
  select(id,SA1_MAINCODE_2016,SSC_CODE_2016 ,SSC_NAME_2016 )

found <- data_base |> st_drop_geometry() |>
  filter(!is.na(IARE_CODE_2016)) |>
  filter(SA1_MAINCODE_2016 %in% missing_IARE$SA1_MAINCODE_2016) |>
  distinct(SA1_MAINCODE_2016,IARE_CODE_2016 ,IARE_NAME_2016)

###duplicated - will need to do via intersect metho
ind <- load_geo(main, layer="indigenous_area_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

missing_IARE <- data_base |>
  filter(is.na(IARE_CODE_2016)) |>
  select(-IARE_CODE_2016 ,-IARE_NAME_2016)


intersects <- intersections(missing_IARE,
                            bigger=ind,
                            base_id="id",
                            bigger_id="IARE_CODE_2016",
                            base_empty_label="SA2_NAME_2016",
                            bigger_empty_label="IARE_NAME_2016"
)

data_base <- data_base |>
  filter(!is.na(IARE_CODE_2016)) |>
  bind_rows(intersects)



#POA ------

missing_poa <- data_base |>
  filter(is.na(POA_CODE_2016)) |>
  select(-POA_CODE_2016,-POA_CODE_2016)

#missing_poa |> distinct(SSC_NAME_2016) |> pull() |> clipr::write_clip()

missing_poa <-
missing_poa |>
  left_join(tribble(~SSC_NAME_2016,~POA_CODE_2016,
"Adelaide Airport",                       "5950",
"Kalka",                                  "0872",
"Anangu Pitjantjatjara Yankunytjatjara",  "0872",
"Pipalyatjara",                           "0872",
"Amata",                                  "0872",
"Pukatja",                                "0872",
"Kaltjiti",                               "0872",
"Mimili",                                 "0872",
"Watarru",                                "0872",
"Garden Island (SA)",                     "5015",
"Torrens Island",                         "5015"
),
by="SSC_NAME_2016")


data_base <- data_base |>
  filter(!is.na(POA_CODE_2016)) |>
  bind_rows(missing_poa) |>
  mutate(POA_NAME_2016=POA_CODE_2016)|>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

## VIC ------
state <- states_to_remediate[6]

data_base <- st_read(here("data-raw","2016_Victoria.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2016,"Migratory",TRUE)) |>
  mutate(Year=2016)

missing |> filter(STE_NAME_2016==state)


data_base |>
  filter(is.na(TR_CODE_2016))

data_base |>
  filter(is.na(CED_CODE_2016))

data_base |>
  filter(is.na(POA_CODE_2016))

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))


### TAS -----

state <- states_to_remediate[5]

data_base <- st_read(here("data-raw","2016_Tasmania.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2016,"Migratory",TRUE)) |>
  mutate(Year=2016)

missing |> filter(STE_NAME_2016==state)

data_base |>
  filter(is.na(TR_CODE_2016))

data_base |>
  filter(is.na(CED_CODE_2016))

data_base |>
  filter(is.na(POA_CODE_2016))

#POA cannot be fixed manually

missing_poas <-
  data_base |>
  filter(is.na(POA_CODE_2016)) |>
  select(-POA_CODE_2016,-POA_NAME_2016)

poa <- load_geo(nonabs,"post_code_area_2016", state=state)


#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2016")
intersects <- intersections(missing_poa,poa, base_id="id",
                            bigger_id="POA_CODE_2016",
                            base_empty_label="id",
                            bigger_empty_label="POA_NAME_2016",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(POA_CODE_2016)) |>
  bind_rows(intersects) |>
  mutate(POA_NAME_2016=POA_CODE_2016)|>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")), delete_dsn = TRUE)

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

# Other Territories

state <- "Other Territories"

data_base <- st_read(here("data-raw","2016_Other Territories.gpkg"))

data_base <- data_base |>
  filter(str_detect(SA2_NAME_2016,"Migratory",TRUE)) |>
  mutate(Year=2016) |>
  mutatE(POA_NAME_2016=POA_CODE_2016)

missing |> filter(STE_NAME_2016==state)

data_base$area <- st_area(data_base)
data_base <-  data_base |> mutate(id=str_c(STE_CODE_2016,"-",row_number()))

st_write(data_base,here("data-raw",str_c("2016_",state,".gpkg")), delete_dsn = TRUE)


## add remainder states to geo structure, calculate proportions -----
geo_structure |> distinct(STE_NAME_2016) |>filter(!(STE_NAME_2016 %in% states_to_remediate))

geo_structure <- bind_rows(st_read(here("data-raw",str_c("2016_New South Wales.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Victoria.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Queensland.gpkg")))|> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Western Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_South Australia.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Northern Territory.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Australian Capital Territory.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Other Territories.gpkg"))) |> st_drop_geometry(),
                           st_read(here("data-raw",str_c("2016_Tasmania.gpkg"))) |> st_drop_geometry())

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
    rename(geo_col="col")

  save_zip_parquet(struct_i,geo_col,here("data-raw","processed"))


}
