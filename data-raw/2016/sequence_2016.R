# get SA1s  ----

## The file provides structure from SA1-SA4- to australia, which builds up without overlaps
#main_layers
base <- load_geo(main, layer = "statistical_area_level_1_2016") %>%
        filter(STE_NAME_2016==state)
sa1_nbr <- nrow(base)

state_boundary <- load_geo(main,layer="state_2016")

# SA2s----

sa <- load_geo(main, layer = "statistical_area_level_2_2016",state=state) %>%
      filter(STE_NAME_2016==state) %>%
      select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=sa,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="SA2_MAINCODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)


intersects <- intersections(base_renmant,
                            bigger=sa,
                            base_id="SA1_MAINCODE_2016",
                            bigger_id="SA2_MAINCODE_2016",
                            base_empty_label="SA1_MAINCODE_2016",
                            bigger_empty_label="SA2_NAME_2016"
)

intersected <- intersects %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)


base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(SA2_NAME_2016))

base <- bind_rows(base,intersects) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

# SA3s----

sa <- load_geo(main, layer = "statistical_area_level_3_2016",state=state) %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=sa,
                              base_id="id",
                              bigger_id="SA3_CODE_2016")

overlapped <- full_overlap %>%
  distinct(id) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="id") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
intersects <- intersections(base_renmant,
                            bigger=sa,
                            base_id="id",
                            bigger_id="SA3_CODE_2016",
                            base_empty_label="SA2_NAME_2016",
                            bigger_empty_label="SA3_NAME_2016"
)

intersected <- intersects %>%
  distinct(id) %>%
  mutate(dummy=TRUE)
}else{
  intersects <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(SA3_NAME_2016))

base <- bind_rows(base,intersects) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))



# SA4s----

sa <- load_geo(main, layer = "statistical_area_level_4_2016",state=state) %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=sa,
                              base_id="id",
                              bigger_id="SA4_CODE_2016")

overlapped <- full_overlap %>%
  distinct(id) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="id") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=sa,
                              base_id="id",
                              bigger_id="SA4_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="SA4_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)
}else{
  intersects <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(SA4_NAME_2016))

base <- bind_rows(base,intersects) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))



rm(sa, intersected,intersects,full_overlap,overlapped,non_matched,base_renmant)
# Indigenous layers ----
#indigenous_layers - iloc ----

ind <- load_geo(indigenous, layer="indigenous_location_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="ILOC_CODE_2016")

overlapped <- full_overlap %>%
              distinct(SA1_MAINCODE_2016) %>%
              mutate(dummy=TRUE)

base_renmant <- base  %>%
                left_join(overlapped,by="SA1_MAINCODE_2016") %>%
                filter(is.na(dummy)) %>%
                select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="ILOC_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="ILOC_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
        left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
        filter(!is.na(ILOC_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
            mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

#indigenous_layers - iare ----

ind <- load_geo(indigenous, layer="indigenous_area_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="IARE_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="IARE_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="IARE_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(IARE_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

#indigenous_layers - ireg ----

ind <- load_geo(indigenous, layer="indigenous_region_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="IREG_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="IREG_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="IREG_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(IREG_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)


# UCL ----

ind <- load_geo(other, layer="urban_centre_and_locality_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="UCL_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="UCL_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="UCL_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(UCL_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)


# SOS ----

ind <- load_geo(other, layer="section_of_state_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="SOS_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="SOS_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="SOS_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(SOS_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

# SOSR ----

ind <- load_geo(other, layer="section_of_state_range_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="SOSR_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="SOSR_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="SOSR_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(SOSR_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

#Tourism Regions -----

ind <- load_geo(nonabs, layer="tourism_region_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

if(nrow(ind)>0){
full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="TR_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="TR_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="TR_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(TR_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))
}
st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

# CED ----

if(exists("ceds_2016_out")){
  base<-base %>%
        left_join(ceds_2016_out,by="SA3_NAME_2016")
}else{
  library(auspol)
  divisions <- list_divisions(filters=list(StateAb=str_to_upper(state_short))) %>%
              filter(`2016`) %>%
    mutate(CED_NAME_2016=str_to_lower(DivisionNm),
           CED_NAME_2016 = str_squish(CED_NAME_2016)) |>
    distinct(CED_NAME_2016,DivisionNm)

  ind <- load_geo(nonabs, layer="commonwealth_electoral_division_2016") %>%
    mutate(CED_NAME_2016=str_to_lower(CED_NAME_2016),
           CED_NAME_2016 = str_squish(CED_NAME_2016)) |>
    left_join(divisions,by="CED_NAME_2016") |>
    filter(!is.na(DivisionNm)) |>
    mutate(CED_NAME_2016=DivisionNm,.keep="unused")

  full_overlap <- full_coverage(base,
                                bigger=ind,
                                base_id="SA1_MAINCODE_2016",
                                bigger_id="CED_CODE_2016")

  overlapped <- full_overlap %>%
    distinct(SA1_MAINCODE_2016) %>%
    mutate(dummy=TRUE)

  base_renmant <- base  %>%
    left_join(overlapped,by="SA1_MAINCODE_2016") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)

  if(nrow(base_renmant)>0){
    intersects <- intersections(base_renmant,
                                bigger=ind,
                                base_id="id",
                                bigger_id="CED_CODE_2016",
                                base_empty_label="SA2_NAME_2016",
                                bigger_empty_label="CED_NAME_2016"
    )

    intersected <- intersects %>%
      distinct(id) %>%
      mutate(dummy=TRUE)

    non_matched <- base_renmant %>%
      left_join(intersected,by="id") %>%
      filter(is.na(dummy)) %>%
      select(-dummy)


  }else{
    intersects <- base %>% filter(is.null(id))
    non_matched <- base %>% filter(is.null(id))
  }

  base <- base %>%
    left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
    filter(!is.na(CED_CODE_2016))

  base <- bind_rows(base,intersects,non_matched) %>%
    mutate(id=row_number())
  sa1_nbr <- c(sa1_nbr,nrow(base))

  st_write_parquet(base,base_file)
  rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)



}
st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,ced2021,intersects,full_overlap,non_matched)


#LGAs ----

ind <- load_geo(nonabs, layer="local_government_area_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="LGA_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="LGA_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="LGA_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(LGA_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

#POAS ----
library(auscensus)
ind <- load_geo(nonabs, layer="post_code_area_2016") %>%
  filter(POA_CODE_2016 %in% as.character(poas_state))

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="POA_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
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


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(POA_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)
## suburbs ----

ind <- load_geo(nonabs, layer="state_suburb_2016") %>%
  filter(STE_NAME_2016==state) %>%
  select(-STE_CODE_2016,-STE_NAME_2016)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_MAINCODE_2016",
                              bigger_id="SSC_CODE_2016")

overlapped <- full_overlap %>%
  distinct(SA1_MAINCODE_2016) %>%
  mutate(dummy=TRUE)

base_renmant <- base  %>%
  left_join(overlapped,by="SA1_MAINCODE_2016") %>%
  filter(is.na(dummy)) %>%
  select(-dummy)

if(nrow(base_renmant)>0){
  intersects <- intersections(base_renmant,
                              bigger=ind,
                              base_id="id",
                              bigger_id="SSC_CODE_2016",
                              base_empty_label="SA2_NAME_2016",
                              bigger_empty_label="SSC_NAME_2016"
  )

  intersected <- intersects %>%
    distinct(id) %>%
    mutate(dummy=TRUE)

  non_matched <- base_renmant %>%
    left_join(intersected,by="id") %>%
    filter(is.na(dummy)) %>%
    select(-dummy)


}else{
  intersects <- base %>% filter(is.null(id))
  non_matched <- base %>% filter(is.null(id))
}

base <- base %>%
  left_join(full_overlap,by="SA1_MAINCODE_2016") %>%
  filter(!is.na(SSC_CODE_2016))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

base <- base |> st_make_valid()

# write ----
#st_write(base,here("data-raw",str_c(state,".geojson")))
st_write(base,here("data-raw",str_c("2016_",state,".gpkg")))
sa1_nbr
