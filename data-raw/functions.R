
load_geo <- function(file, layer,state_label="STATE_NAME_2021",state=NULL){

  geo <-st_read(file, layer = layer)
  col_names <- colnames(geo)
  if(!is.null(state)){
  if(state_label %in% col_names){
    geo <- geo %>%
      filter(if_any(c(state_label), ~ .x==state))
  }}

  geo <- geo %>%
    select(-starts_with("AREA_ALBERS"))


  return(geo)

}

full_coverage <-function(base,bigger,base_id,bigger_id,divisions=10){

  base_cols   <- colnames(base)
  bigger_cols <- colnames(bigger)
  remnant     <- bigger_cols[!(bigger_cols %in% base_cols)]

  bigger_ids <- bigger %>%
    select(any_of(remnant)) %>%
    st_drop_geometry()


  #p <- progressor(nrow(bigger_ids))
  results <- NULL
  aux_seq <- floor(seq(1,nrow(bigger_ids),length.out=divisions))
  print("divisions")
  print(aux_seq)

  for(i in 1:nrow(bigger_ids)){

    if(i %in% aux_seq){
      print(str_c(i, " out of ",nrow(bigger_ids)))
    }

    bigger_row <- bigger_ids[i,] %>% select(any_of(bigger_id)) %>% pull()

    indexes <- st_covered_by(base ,
                             bigger %>%  filter(if_any(c(bigger_id), ~ .x==bigger_row)),
                             sparse = FALSE)

    results_i  <- base[indexes==TRUE,] %>%
      select(any_of(c(base_id))) %>%
      st_drop_geometry()

    if(nrow(results_i)>0){

      results_i <- expand_grid(results_i,bigger_ids[i,])
      results <- bind_rows(results_i,results)

    }

  }
  return(results)
}

intersections  <- function(base_renmant,bigger,base_id,bigger_id,base_empty_label,bigger_empty_label,threshold=0.03){

  base_cols   <- colnames(base_renmant)
  bigger_cols <- colnames(bigger)
  remnant     <- bigger_cols[!(bigger_cols %in% base_cols)]

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
      filter(if_any(c(base_id), ~ .x %in% as.vector(b))) %>%
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
                                 dimension = c("polygon")) %>%
        st_make_valid()


      inter_i$is_polygon <- st_is(inter_i,c("POLYGON","MULTIPOLYGON"))

      inter_i <- inter_i %>%
        filter(is_polygon) %>%
        select(-is_polygon)

      if(nrow(inter_i)>1){
        inter_i$area <- inter_i %>% st_area()

       inter_i <- inter_i %>%
          mutate(area_prop =as.numeric(area/sum(area,na.rm = TRUE))) %>%
          filter(area_prop>threshold) %>%
          select(-area,-area_prop)
      }

      if(nrow(inter_i)==1){



        inter_i <- base_renmant[i,] %>%
                   left_join(inter_i %>%
                            st_drop_geometry() %>%
                            select(any_of(base_id),any_of(remnant)),
                            by=base_id)



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
