# ASGC 2006 already comes with the job done!  ----

## The file provides structure from SA1-SA4- to australia, which builds up without overlaps
#main_layers
#https://www.abs.gov.au/ausstats/abs@.nsf/bb8db737e2af84b8ca2571780015701e/4f5e80e179da7a07ca25720a00081259!OpenDocument
base <- load_geo(main, layer = "census_collection_district_2006") %>%
        filter(STE_NAME_2006==state) %>%
        mutate(id=row_number())

# write ----
#st_write(base,here("data-raw",str_c(state,".geojson")))
st_write(base,here("data-raw",str_c("2006_",state,".gpkg")))

