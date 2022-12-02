# setup, aux ----
library(here)
library(fs)
library(sf)
library(tidyverse)
library(arrow)
library(sfarrow)

load_geo <- function(file, layer,state_label="STATE_NAME_2021",state=NULL){

  geo <-st_read(file, layer = layer)
  col_names <- colnames(geo)

  if(state_label %in% col_names){
    geo <- geo %>%
     filter(if_any(c(state_label), ~ .x==state))
  }

    geo <- geo %>%
    select(-starts_with("AREA_ALBERS"))


  return(geo)

}

full_coverage <-function(base,bigger,base_id,bigger_id){

  base_cols   <- colnames(base)
  bigger_cols <- colnames(bigger)
  remnant     <- bigger_cols[!(bigger_cols %in% base_cols)]

  bigger_ids <- bigger %>%
    select(any_of(remnant)) %>%
    st_drop_geometry()


  for(i in 1:nrow(bigger_ids)){
    bigger_row <- bigger_ids[i,] %>% select(any_of(bigger_id)) %>% pull
    print(str_c(i, " out of ",nrow(bigger_ids)))
    indexes <- st_covered_by(base ,
                             bigger %>%  filter(if_any(c(bigger_id), ~ .x==bigger_row)),
                             sparse = FALSE)

    results_i  <- base[indexes==TRUE,] %>%
      select(any_of(c(base_id))) %>%
      st_drop_geometry()

    if(nrow(results_i)>0){

        results_i <- expand_grid(results_i,bigger_ids[i,])

        if(!exists("results")){
            results <- results_i
        }else{
            results <- bind_rows(results_i,results)
        }
    }

  }
  return(results)
}

intersections  <- function(base_renmant,bigger,base_id,bigger_id,base_empty_label,bigger_empty_label){

  base_cols   <- colnames(base_renmant)
  bigger_cols <- colnames(bigger)
  remnant     <- bigger_cols[!(bigger_cols %in% base_cols)]
  threshold   <- 0.03

  a <- st_intersects(base_renmant,bigger, sparse = FALSE) %>% as.data.frame(.)

  colnames(a) <- bigger %>%
    st_drop_geometry() %>%
    select(bigger_id) %>% pull(.)

  a$small <- base_renmant %>%
    st_drop_geometry()%>%
    select(any_of(base_id)) %>%
    pull()

  colnames(a)[which(colnames(a)=="small")] <- base_id

  a <- a %>%
    pivot_longer(-any_of(c(base_id)),
                 names_to=bigger_id, values_to = "value") %>%
    filter(value)


  inter <- NULL

  for(i in 1:nrow(base_renmant)){

  print(str_c(i," out of ",nrow(base_renmant)))
  b <- base_renmant[i,] %>%
      st_drop_geometry() %>%
      select(any_of(base_id)) %>%
      pull()


  c <- a %>%
          filter(if_any(c(base_id), ~ .x %in% b)) %>%
          select(any_of(bigger_id)) %>%
          pull()

  if(length(c)==0){
    print("empty path!")
    matching_label <- base_renmant[i,] %>%
                      st_drop_geometry() %>%
                       select(any_of(base_empty_label)) %>%
                       pull() %>%
                       str_remove_all(.,"\\((.*?)\\)")

    bigger_eq <- bigger %>%
                  select(any_of(remnant)) %>%
                   filter(if_any(bigger_empty_label, ~ str_detect(.x,matching_label))) %>%
                    st_drop_geometry()


    inter_i <- expand_grid(base_renmant[i,],bigger_eq)



  }else{
  inter_i <- st_intersection(base_renmant[i,],
                           bigger %>%
                             select(any_of(remnant)) %>%
                             filter(if_any(bigger_id, ~ .x %in% c)),
                           dimension = "polygon") %>%
                             st_make_valid()


  inter_i$is_polygon <- st_is(inter_i,c("POLYGON","MULTIPOLYGON"))

  inter_i <- inter_i %>%
             filter(is_polygon) %>%
             select(-is_polygon)


  if(nrow(inter_i)!=1){

    inter_i$area <- inter_i %>% st_area()

    inter_i <- inter_i %>%
      mutate(area_prop =as.numeric(area/sum(area,na.rm = TRUE))) %>%
      filter(area_prop>threshold) %>%
      select(-area,-area_prop)
  }

  }

  if(is.null(inter)){

    inter <- inter_i
  }else{
    inter <- bind_rows(inter,inter_i)
  }

}
  return(inter)


}

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

state <- "Victoria"
temp_file <- here("data-raw","temp_df.parquet")
base_file <- here("data-raw","base_sf2.parquet")
ced_file <- here("data-raw","source","divisions-Aug-2021-by-2016-SA1")

# get SA1s  ----

## The file provides structure from SA1-SA4- to australia, which builds up without overlaps
#main_layers
base <- load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
# Indigenous layers ----
#indigenous_layers

indigenous <- load_geo(indigenous, layer="ILOC_2021_AUST_GDA2020",state=state)

full_overlap <- full_coverage(base,
                              bigger=indigenous,
                              base_id="SA1_CODE_2021",
                              bigger_id="ILOC_CODE_2021")

base_renmant <- base  %>%
               filter(!(SA1_CODE_2021 %in% full_overlap$SA1_CODE_2021))


intersects <- intersections(base_renmant,
                            bigger=indigenous,
                            base_id="SA1_CODE_2021",
                            bigger_id="ILOC_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="ILOC_NAME_2021"
                            )


base <- base %>%
        left_join(full_overlap,by="SA1_CODE_2021") %>%
        filter(!is.na(ILOC_CODE_2021))

base <- bind_rows(base,intersects)
st_write_parquet(base,base_file)
rm(base_renmant,indigenous,intersects,full_overlap)

## UCL

ucl  <- load_geo(other, layer="UCL_2021_AUST_GDA2020",state=state)


full_overlap <- full_coverage(base,
                              bigger=ucl,
                              base_id="SA1_CODE_2021",
                              bigger_id="UCL_CODE_2021")

base_renmant <- base  %>%
  filter(!(SA1_CODE_2021 %in% full_overlap$SA1_CODE_2021))


intersects <- intersections(base_renmant,
                            bigger=ucl,
                            base_id="SA1_CODE_2021",
                            bigger_id="UCL_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="UCL_NAME_2021")

non_matched <- base %>%
              filter(if_any(c("SA1_CODE_2021"), ~ !(.x %in% unique(c(full_overlap$SA1_CODE_2021,
                                                                     intersects %>% st_drop_geometry() %>%
                                                                       select(SA1_CODE_2021) %>%
                                                                       pull()
                                                                     )))))


base <- base %>%
  left_join(full_overlap,by="SA1_CODE_2021") %>%
  filter(!is.na(UCL_CODE_2021))

base <- bind_rows(base,intersects)

st_write_parquet(base,base_file)
rm(base_renmant,ucl,intersects,full_overlap,neighbouring,non_matched)


## suburbs

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
        mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))


LGA_2022_AUST_GDA2020



