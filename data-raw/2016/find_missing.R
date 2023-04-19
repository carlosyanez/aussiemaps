
b$empty <- b |> st_is_empty()
b <- b |> filter(!empty) |> select(-empty)
base <- NULL
for(i in 1:nrow(b)){
  redo_flag <- TRUE
  sa1 <- b[i,]
  sa_code <- sa1 |> st_drop_geometry() |> pull(SA1_MAINCODE_2016 )

  existing <- data_base |> filter(SA1_MAINCODE_2016 ==sa_code)
  existing <- tibble(a="sa_code",geom=st_union(existing)) |>
    st_as_sf() |>
    smoothr::fill_holes(units::set_units(1,"km^2"))
  sa1 <- sa1 |> st_make_valid()

  tryCatch({
    diff <-abs(as.numeric((st_area(existing)- st_area(sa1))/st_area(sa1)))

    if(diff<10^5){
      message(glue::glue("{i} out of {nrow(b)}: Equal Area"))
    }else{
    base_i <- st_difference(existing,sa1)
    base_i <- st_cast(base_i, "POLYGON")

    base_i$area <- st_area(base_i)
    base_i <- base_i |> filter(area > units::set_units(100,"m^2"))

    message(glue::glue("{i} out of {nrow(b)}: {sa_code}. {nrow(base_i)} features"))
    }
    redo_flag <- FALSE
  },
  error=function(e){message(glue:::glue("{i} out of {nrow(b)}: didnt work"))})

  if(redo_flag){
    base_i <- sa1
    data_base <- data_base |> filter(SA1_MAINCODE_2016 !=sa_code)

  }

  if(nrow(base_i)==0){
    message("no missing bits")
  }else{
  if(is.null(base)){
    base <- base_i
  }else{
    base <- bind_rows(base,base_i)
  }
  }
}




base <- base[st_is(base |> st_make_valid(),c("POLYGON","MULTIPOLYGON")),]
base <- base |> select(-a,-area)
keep_vars <- unique(c(ls(),"keep_vars"))
base <- base |> rename("geom"="shape")
