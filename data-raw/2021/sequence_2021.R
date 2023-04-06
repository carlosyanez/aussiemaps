# get SA1s  ----

## The file provides structure from SA1-SA4- to australia, which builds up without overlaps
#main_layers
if(!exists("base")){
base <- load_geo(main, layer = "SA1_2021_AUST_GDA2020",state=state)
}else{
  message("base exists!")
}
sa1_nbr <- nrow(base)

state_boundary <- load_geo(main,layer="STE_2021_AUST_GDA2020",state=state)

# Indigenous layers ----
#indigenous_layers

ind <- load_geo(indigenous, layer="ILOC_2021_AUST_GDA2020",state=state)

full_overlap <- full_coverage(base,
                              bigger=ind,
                              base_id="SA1_CODE_2021",
                              bigger_id="ILOC_CODE_2021")

overlapped <- full_overlap %>%
              distinct(SA1_CODE_2021) %>%
              mutate(dummy=TRUE)

base_renmant <- base  %>%
                left_join(overlapped,by="SA1_CODE_2021") %>%
                filter(is.na(dummy)) %>%
                select(-dummy)


intersects <- intersections(base_renmant,
                            bigger=ind,
                            base_id="SA1_CODE_2021",
                            bigger_id="ILOC_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="ILOC_NAME_2021",
                            threshold = threshold
                            )

intersected <- intersects %>%
                distinct(SA1_CODE_2021) %>%
                mutate(dummy=TRUE)

non_matched <- base_renmant %>%
                left_join(intersected,by="SA1_CODE_2021") %>%
                filter(is.na(dummy)) %>%
                select(-dummy)


base <- base %>%
        left_join(full_overlap,by="SA1_CODE_2021") %>%
        filter(!is.na(ILOC_CODE_2021))

base <- bind_rows(base,intersects) %>%
            mutate(id=row_number())
sa1_nbr <- c(sa1_nbr,nrow(base))

st_write_parquet(base,base_file)
rm(base_renmant,ind,intersects,full_overlap,overlapped,intersected,non_matched)

## UCL ----

ucl  <- load_geo(other, layer="UCL_2021_AUST_GDA2020",state=state)


full_overlap <- full_coverage(base,
                              bigger=ucl,
                              base_id="id",
                              bigger_id="UCL_CODE_2021",
                              divisions = 347)

base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id)) %>%
  mutate(id=row_number())


intersects <- intersections(base_renmant,
                            bigger=ucl,
                            base_id="id",
                            bigger_id="UCL_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="UCL_NAME_2021",
                            threshold = threshold)

non_matched <- base %>%
              filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                                     intersects %>% st_drop_geometry() %>%
                                                                       select(id) %>%
                                                                       pull()
                                                                     )))))


base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(UCL_CODE_2021))

base <- bind_rows(base,intersects) %>%
  mutate(id=row_number())

st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,ucl,intersects,full_overlap,non_matched)

#non abs, from bigger to smaller structures

#Tourism Regions ----

tr <- load_geo(nonabs,"TR_2021_AUST_GDA2020", state=state)

full_overlap <- full_coverage(base,
                              bigger=tr,
                              base_id="id",
                              bigger_id="TR_CODE_2021")

base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id))


intersects <- intersections(base_renmant,
                            bigger=tr,
                            base_id="id",
                            bigger_id="TR_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="TR_NAME_2021",
                            threshold = threshold
)


non_matched <- base %>%
  filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                         intersects %>% st_drop_geometry() %>%
                                                           select(id) %>%
                                                           pull()
  )))))

base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(TR_CODE_2021))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())

st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,tr,intersects,full_overlap,non_matched)

# CED ----

# CED 2021 ----

if(exists("ceds_2021_out")){
  base<-base %>%
        left_join(ceds_2021_out,by="SA3_NAME_2021")
}else{
  ced2021 <- load_geo(nonabs,"CED_2021_AUST_GDA2020", state=state)

full_overlap <- full_coverage(base,
                              bigger=ced2021,
                              base_id="id",
                              bigger_id="CED_CODE_2021")

base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id))


intersects <- intersections(base_renmant,
                            bigger=ced2021,
                            base_id="id",
                            bigger_id="CED_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="CED_NAME_2021",
                            threshold = threshold
)

non_matched <- base %>%
  filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                         intersects %>% st_drop_geometry() %>%
                                                           select(id) %>%
                                                           pull()
  )))))


base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(CED_CODE_2021))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
}
st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,ced2021,intersects,full_overlap,non_matched)


#LGAs ----

lga <- load_geo(nonabs,"LGA_2021_AUST_GDA2020", state=state)

full_overlap <- full_coverage(base,
                              bigger=lga,
                              base_id="id",
                              bigger_id="LGA_CODE_2021")


base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id))


intersects <- intersections(base_renmant,
                            bigger=lga,
                            base_id="id",
                            bigger_id="LGA_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="LGA_NAME_2021",
                            threshold = threshold
)

non_matched <- base %>%
  filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                         intersects %>% st_drop_geometry() %>%
                                                           select(id) %>%
                                                           pull()
  )))))


base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(LGA_NAME_2021))

base <- bind_rows(base,intersects,non_matched) %>%
  mutate(id=row_number())
st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,lga,intersects,full_overlap,non_matched)

#POAS ----

poa <- load_geo(nonabs,"POA_2021_AUST_GDA2020", state=state) %>%
        filter(POA_NAME_2021 %in% poas_state)

full_overlap <- full_coverage(base,
                              bigger=poa,
                              base_id="id",
                              bigger_id="POA_CODE_2021")



base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id))


intersects <- intersections(base_renmant,
                            bigger=poa,
                            base_id="id",
                            bigger_id="POA_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="POA_NAME_2021",
                            threshold = threshold
)

non_matched <- base %>%
  filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                         intersects %>% st_drop_geometry() %>%
                                                           select(id) %>%
                                                           pull()
  )))))

if(!is.null(full_overlap)){
base <- base %>%
  left_join(full_overlap,by="id") %>%
  filter(!is.na(POA_NAME_2021))

base <- bind_rows(base,intersects,non_matched)
}else{
  base <- bind_rows(intersects, non_matched)
}
base <- base %>%
  mutate(id=row_number())

st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,poa,intersects,full_overlap,non_matched)

## suburbs ----
base<- st_as_sf(base)

sal <- load_geo(nonabs,"SAL_2021_AUST_GDA2020", state=state) %>%
        mutate(SAL_NAME_2021=str_remove_all(SAL_NAME_2021,"\\((.*?)\\)"))

full_overlap_i <- list()

split <- c(seq(1,nrow(base),by=100),nrow(base))
split <- unique(split)

for(i in 1:(length(split)-1)){
  print(str_c(i," out of ",length(split)))

  start <- split[i]
  end   <- if_else(i+1==length(split),split[i+1],split[i+1]-1)

  full_overlap_i[[i]] <- full_coverage(base[start:end,],
                                      bigger=sal,
                                     base_id="id",
                                     bigger_id="SAL_CODE_2021")

}

full_overlap <- tibble()
if(length(full_overlap_i)>0)){
for(i in 1:length(full_overlap_i)){
  full_overlap <- bind_rows(full_overlap,full_overlap_i)
}

full_overlap <- full_overlap %>% distinct()

base_renmant <- base  %>%
  filter(!(id %in% full_overlap$id))
}else{
  base_renmant <-base
}


intersects <- intersections(base_renmant,
                            bigger=sal,
                            base_id="id",
                            bigger_id="SAL_CODE_2021",
                            base_empty_label="SA2_NAME_2021",
                            bigger_empty_label="SAL_NAME_2021",
                            threshold = threshold
)

non_matched <- base %>%
  filter(if_any(c("id"), ~ !(.x %in% unique(c(full_overlap$id,
                                                         intersects %>% st_drop_geometry() %>%
                                                           select(id) %>%
                                                           pull()
  )))))

base <- base %>%
  mutate(id=row_number())

if(nrow(full_overlap)==0) full_overlap <- NULL

if(!is.null(full_overlap)){
  base <- base %>%
    left_join(full_overlap,by="id") %>%
    filter(!is.na(POA_NAME_2021))

  base <- bind_rows(base,intersects,non_matched)
}else{
  base <- bind_rows(intersects |> select(-any_of(c("id"))), non_matched |> select(-any_of(c("id"))))
}



st_write_parquet(base,base_file)
sa1_nbr <- c(sa1_nbr,nrow(base))
rm(base_renmant,sed,intersects,full_overlap,non_matched)

base <- base |> st_make_valid()

# write ---
#st_write(base,here("data-raw",str_c(state,".geojson")))
if(!exists("dont_write")){
st_write(base,here("data-raw",str_c("2021_",state,".gpkg")))
}
sa1_nbr

