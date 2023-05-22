
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){
  redo_flag <- TRUE
  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_MAINCODE_2011 )

  existing <- data_base |> filter(SA1_MAINCODE_2011==sa_code)

  if(nrow(existing)>0){

  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))
  existing <- existing |> st_make_valid()
  sa1 <- sa1 |> st_make_valid()

  tryCatch({
    exist_area  <- as.numeric(st_area(existing))
    sa1_area    <- as.numeric(st_area(sa1))

    diff        <- abs((exist_area-sa1_area)/sa1_area)

    if(diff<10^-12){
      message(glue::glue("{i} out of {nrow(b)}: Equal Area"))
      base_i<-tibble()
    }else{

    if(exist_area<sa1_area){
      base_i <- st_difference(sa1,existing)
    }else{
      base_i <- st_difference(existing,sa1)
    }

    base_i <- st_cast(base_i, "POLYGON")

    base_i$area <- st_area(base_i)
    base_i <- base_i |> filter(area > units::set_units(5,"m^2"))

    message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
    }
    redo_flag <- FALSE
  },
  error=function(e){message(glue:::glue("{i} out of {nrow(b)}: didnt work"))})
  if(redo_flag){
    base_i <- sa1
    data_base <- data_base |> filter(SA1_MAINCODE_2011 !=sa_code)

  }
  }else{
    message(glue::glue(("{i} out of {nrow(b)}: missing SA1")))
    base_i <- sa1
  }
  if(nrow(base_i)>0){
    if(any(str_detect(colnames(base_i),"shape"))) base_i <- base_i |> rename("geom"="shape")

  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }
  }
}

if(!is.null(base)){
base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))
}
