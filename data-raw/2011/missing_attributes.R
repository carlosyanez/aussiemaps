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


main_layers       <- rgdal::ogrListLayers(main)
nonabs_layers     <- rgdal::ogrListLayers(nonabs)




files <- dir_ls(here("data-raw"),regexp = "gpkg")
files <- dir_ls(aussiemaps::find_maps_cache(),regexp = "gpkg")
files <- files[str_detect(files,"\\/2011")]
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

### ACT-----

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
  select(-SAL_CODE_2021,SAL_NAME_2021)

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
    mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")
intersects <- intersections(missing_sal,sal, base_id="id",
                                                          bigger_id="SAL_CODE_2021",
                                                          base_empty_label="id",
                                                          bigger_empty_label="SAL_NAME_2021",
                                                          threshold = 0
)

data_base <- data_base |>
  filter(!is.na(SAL_CODE_2021)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base <- st_difference(data_base)

data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

data_base$area <- st_area(data_base)


## geo-structure, save -----
st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())



## NT ------
state <- states_to_remediate[2]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_Northern.Territory.gpkg")

#just missing postcodes , can be written manually

data_base |> st_drop_geometry() |>
  filter(is.na(POA_CODE_2021)) |>
  distinct(SAL_NAME_2021)

data_base <- data_base |>
            mutate(across(any_of(c("POA_CODE_2021","POA_NAME_2021")),
                          ~ case_when(
                            is.na(.x) & SAL_NAME_2021=="Araluen" ~ "0870",
                            TRUE ~ .x
                          )))

data_base <- st_difference(data_base)
data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

data_base$area <- st_area(data_base)


st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## NSW ------
state <- states_to_remediate[3]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_New.South.Wales.gpkg")

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
  mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

missing_sal <- data_base |>
  filter(is.na(SAL_CODE_2021)) |>
  select(-SAL_CODE_2021,-SAL_NAME_2021)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")
intersects <- intersections(missing_sal,sal, base_id="id",
                            bigger_id="SAL_CODE_2021",
                            base_empty_label="id",
                            bigger_empty_label="SAL_NAME_2021",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(SAL_CODE_2021)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base <- st_difference(data_base)

data_base$area <- st_area(data_base)

data_base |> filter(area=units::set_units(0,"m^2"))

data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

dedup_cols <- data_base[1,] |> st_drop_geometry() |> colnames()
dedup_cols <- dedup_cols[str_detect(dedup_cols,"CODE")]
dedup_cols <- c(dedup_cols,"area")

dup <- data_base |> st_drop_geometry() |> count(across(any_of(dedup_cols))) |> filter(n>1)

d2 <- data_base |> left_join(dup,by=dedup_cols) |> filter(!is.na(n))
dup_d2 <- d2 |> st_drop_geometry() |> select(id)

d2 <- d2 |>
      select(-n) |>
      mutate(id=row_number()) |>
      group_by(across(any_of(dedup_cols))) |>
      mutate(min=(id==min(id))) |>
      ungroup() |>
      filter(min) |>
      select(-min)

data_base <- data_base |>
  filter(!(id %in% dup_d2)) |>
  select(-id) |>
  bind_rows(d2 |> select(-id)) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  st_make_valid()

st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry())

## QLD ------
state <- states_to_remediate[4]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_Queensland.gpkg")

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
  mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

missing_sal <- data_base |>
  filter(is.na(SAL_CODE_2021)) |>
  select(-SAL_CODE_2021,-SAL_NAME_2021)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")
intersects <- intersections(missing_sal,sal, base_id="id",
                            bigger_id="SAL_CODE_2021",
                            base_empty_label="id",
                            bigger_empty_label="SAL_NAME_2021",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(SAL_CODE_2021)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)

data_base |> filter(area==units::set_units(0,"m^2"))

data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

dedup_cols <- data_base[1,] |> st_drop_geometry() |> colnames()
dedup_cols <- dedup_cols[str_detect(dedup_cols,"CODE")]
dedup_cols <- c(dedup_cols,"area")

dup <- data_base |> st_drop_geometry() |> count(across(any_of(dedup_cols))) |> filter(n>1)

d2 <- data_base |> left_join(dup,by=dedup_cols) |> filter(!is.na(n))
dup_d2 <- d2 |> st_drop_geometry() |> select(id)

d2 <- d2 |>
  select(-n) |>
  mutate(id=row_number()) |>
  group_by(across(any_of(dedup_cols))) |>
  mutate(min=(id==min(id))) |>
  ungroup() |>
  filter(min) |>
  select(-min)

data_base <- data_base |>
  filter(!(id %in% dup_d2)) |>
  select(-id) |>
  bind_rows(d2 |> select(-id)) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  st_make_valid()


st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))


## SA ------
state <- states_to_remediate[5]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_South.Australia.gpkg")

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
  mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

missing_sal <- data_base |>
  filter(is.na(SAL_CODE_2021)) |>
  select(-SAL_CODE_2021,-SAL_NAME_2021)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")
intersects <- intersections(missing_sal,sal, base_id="id",
                            bigger_id="SAL_CODE_2021",
                            base_empty_label="id",
                            bigger_empty_label="SAL_NAME_2021",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(SAL_CODE_2021)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)

data_base |> filter(area==units::set_units(0,"m^2"))

data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

dedup_cols <- data_base[1,] |> st_drop_geometry() |> colnames()
dedup_cols <- dedup_cols[str_detect(dedup_cols,"CODE")]
dedup_cols <- c(dedup_cols,"area")

dup <- data_base |> st_drop_geometry() |> count(across(any_of(dedup_cols))) |> filter(n>1)

d2 <- data_base |> left_join(dup,by=dedup_cols) |> filter(!is.na(n))
dup_d2 <- d2 |> st_drop_geometry() |> select(id)

d2 <- d2 |>
  select(-n) |>
  mutate(id=row_number()) |>
  group_by(across(any_of(dedup_cols))) |>
  mutate(min=(id==min(id))) |>
  ungroup() |>
  filter(min) |>
  select(-min)

data_base <- data_base |>
  filter(!(id %in% dup_d2)) |>
  select(-id) |>
  bind_rows(d2 |> select(-id)) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  st_make_valid()


st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

## VIC ------
state <- states_to_remediate[6]
missing |> filter(STATE_NAME_2021==state)
data_base <- load_aussiemaps_gpkg("2021_Victoria.gpkg")

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
  mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

missing_sal <- data_base |>
  filter(is.na(SAL_CODE_2021)) |>
  select(-SAL_CODE_2021,-SAL_NAME_2021)

#full_coverage <- full_coverage(missing_sal,sal,"id","SAL_NAME_2021")
intersects <- intersections(missing_sal,sal, base_id="id",
                            bigger_id="SAL_CODE_2021",
                            base_empty_label="id",
                            bigger_empty_label="SAL_NAME_2021",
                            threshold = 0
)

data_base <- data_base |>
  filter(!is.na(SAL_CODE_2021)) |>
  bind_rows(intersects) |>
  st_make_valid()

data_base$empty <- st_is_empty(data_base)

data_base <- data_base |>
  filter(!empty) |> select(-empty)

data_base <- data_base |> st_make_valid()
data_base <- st_cast(data_base,"POLYGON")

data_base$area <- st_area(data_base)

data_base |> filter(area==units::set_units(0,"m^2"))

data_base <-  data_base |> mutate(id=str_c(STATE_CODE_2021,"-",row_number()))

dedup_cols <- data_base[1,] |> st_drop_geometry() |> colnames()
dedup_cols <- dedup_cols[str_detect(dedup_cols,"CODE")]
dedup_cols <- c(dedup_cols,"area")

dup <- data_base |> st_drop_geometry() |> count(across(any_of(dedup_cols))) |> filter(n>1)

d2 <- data_base |> left_join(dup,by=dedup_cols) |> filter(!is.na(n))
dup_d2 <- d2 |> st_drop_geometry() |> select(id)

d2 <- d2 |>
  select(-n) |>
  mutate(id=row_number()) |>
  group_by(across(any_of(dedup_cols))) |>
  mutate(min=(id==min(id))) |>
  ungroup() |>
  filter(min) |>
  select(-min)

data_base <- data_base |>
  filter(!(id %in% dup_d2)) |>
  select(-id) |>
  bind_rows(d2 |> select(-id)) |>
  mutate(id=str_c(STATE_CODE_2021,"-",row_number())) |>
  st_make_valid()


st_write(data_base,here("data-raw",str_c("2021_",state,".gpkg")))

geo_structure <- bind_rows(geo_structure,
                           data_base |> st_drop_geometry() |> mutate(area=as.numeric(area)))

## add remainder states to geo structure, calculate proportions -----
geo_structure |> distinct(STATE_NAME_2021) |>filter(!(STATE_NAME_2021 %in% states_to_remediate))

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

# NSW
nsw <- st_read(here("data-raw","2021_New South Wales.gpkg"))

half_way <- floor(nrow(nsw)/8)

nsw1 <- nsw[1:(4*half_way),]
nsw2 <- nsw[(4*half_way+1):(5*half_way),]
nsw3 <- nsw[(5*half_way+1):(6*half_way),]
nsw4 <- nsw[(6*half_way+1):(7*half_way),]
nsw5 <- nsw[(7*half_way+1):nrow(nsw),]


nrow(nsw) == (nrow(nsw1)+nrow(nsw2)+nrow(nsw3)+nrow(nsw4)+nrow(nsw5))
st_write(nsw1,here("data-raw","2021_New South Wales 1.gpkg"))
st_write(nsw2,here("data-raw","2021_New South Wales 2.gpkg"))
st_write(nsw3,here("data-raw","2021_New South Wales 3.gpkg"))
st_write(nsw4,here("data-raw","2021_New South Wales 4.gpkg"))
st_write(nsw5,here("data-raw","2021_New South Wales 5.gpkg"))



save_zip_gpkg(here("data-raw","2021_New South Wales 1.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
save_zip_gpkg(here("data-raw","2021_New South Wales 2.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
save_zip_gpkg(here("data-raw","2021_New South Wales 3.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
save_zip_gpkg(here("data-raw","2021_New South Wales 4.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
save_zip_gpkg(here("data-raw","2021_New South Wales 5.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
