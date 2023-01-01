#' Get sf object containing selected map polygons
#' @return sf object with selected polygons
#' @importFrom arrow read_parquet
#' @importFrom dplyr mutate across select any_of filter if_any pull group_by starts_with left_join  if_else n everything matches relocate last_col
#' @importFrom stringr str_remove_all str_detect str_c str_extract str_replace str_replace_all
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_as_sf st_union st_make_valid sf_use_s2 st_drop_geometry
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom units set_units
#' @importFrom smoothr fill_holes
#' @param  filter_table table to filter (you can start with location_table)
#' @param  filters list with filters
#' @param  year year
#' @param  aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  simplification_factor  0-1 simplication threshold to pass to rmapshaper::ms_simplify()
#' @param  smoothing_threshold smoothing threshold (default to 1, as in km^2)
#' @export
get_map <- function(filter_table=NULL,
                    filters=NULL,
                    year,
                    aggregation=NULL,
                    simplification_factor=1,
                    smoothing_threshold=4){

  if(is.null(filter_table)&is.null(filters)) stop("Either filter table or filters need to be provided")

  #get filter table if not provided
  if(is.null(filter_table)){
    filter_table <- list_structure(year,filters) |>
      select(-matches("AREA_ALBERS_SQKM")) |>
      select(-matches("\\."))
  }

  cache_dir  <- find_maps_cache()

  file_regex <- str_c(year,"_[A-Z]{1}")

  repo_base      <- get_repo_files() |>
                mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
                select(any_of("file_name"))

  repo      <- repo_base |>
                filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
                pull()


  state_col <- colnames(filter_table)[str_detect(colnames(filter_table),"STATE|STE")]
  state_col <- state_col[str_detect(state_col,"NAME")]


  required_states <- filter_table |>
                     select(all_of(c(state_col))) |>
                     distinct() |>
                     mutate(across(all_of(as.vector(state_col)), ~ str_replace_all(.x," ","\\."))) |>
                     mutate(across(all_of(as.vector(state_col)), ~ str_c(year,"_",.x))) |>
                     filter(if_any(all_of(as.vector(state_col)), ~ .x %in% repo))           |>
                     pull()

  data_sf <- NULL

  for(repo_i in required_states){
    data_i <- suppressMessages(suppressWarnings(load_aussiemaps_gpkg(repo_i,filter_table))) |>
              select(-matches("AREA_ALBERS_SQKM")) |>
              select(-matches("\\."))

    data_sf <- bind_rows(data_sf,data_i)

  }

  rm(data_i)

  #aggregate

  if(!is.null(aggregation)){

    aggregation <- as.vector(aggregation)

    for(i in 1:length(aggregation)){

    aggregation_prefix <- str_extract(aggregation[i],"^[^_]*")
    aggregation_suffix <- str_extract(aggregation[i],"[0-9]{4}")

    aggregation[i]  <- repo_base |>
      filter(if_any(c("file_name"), ~ str_detect(.x,aggregation_prefix))) |>
      filter(if_any(c("file_name"), ~ str_detect(.x,as.character(aggregation_suffix)))) |>
      filter(if_any(c("file_name"), ~ str_detect(.x,"CODE"))) |>
      head(1) |>
      pull()

    }

    external_territories <- any(str_detect(required_states,"Other"))

    if(external_territories){
      data_sf_external  <- data_sf |> filter(if_any(as.vector(state_col), ~ str_detect(.x,"Other")))
      data_sf           <- data_sf |> filter(if_any(as.vector(state_col), ~ str_detect(.x,"Other"),TRUE))
    }

    #decide what to keep

    cols_to_keep <-filter_table |>
      mutate(across(everything(), as.character)) |>
      select(-any_of(c("id","area","Year"))) |>
      pivot_longer(-any_of(aggregation),values_to = "value",names_to = "geo_unit") |>
      distinct() |>
      group_by(across(all_of(c(aggregation,"geo_unit")))) |>
      summarise(n=n(),.groups="drop") |>
      group_by(across(all_of(c("geo_unit")))) |>
      summarise(n=mean(n),.groups="drop") |>
      filter(if_any(c("n"), ~ .x==1)) |>
      select(any_of("geo_unit")) |>
      distinct() |>
      pull()

    cols_to_keep <- c(cols_to_keep,"Year")

    #columns to merge

    cols_to_merge <- colnames(filter_table)
    cols_to_merge <- cols_to_merge[!(cols_to_merge %in% c(cols_to_keep,aggregation,"id","area"))]

    #new aggregated sum

    areas_prop <- load_aussiemaps_parquet(aggregation) |>
                  filter(if_any(c("id"), ~ .x %in% filter_table$id)) |>
                  collect() |>
                  group_by(across(c("geo_col")))   |>
                  summarise(across(any_of("prop"), ~ sum(.x)),.groups="drop")

    sf_use_s2(FALSE)
    data_sf <- suppressMessages(suppressWarnings(data_sf |>
                                                group_by(across(c(aggregation,cols_to_keep))) |>
                                                summarise(.groups="drop") |>
                                                st_make_valid() |>
                                                st_union(by_feature = TRUE) |>
                                                fill_holes(set_units(smoothing_threshold,"km^2"))
    ))


    for(col_to_merge  in cols_to_merge){

    merged_col  <- filter_table |>
                   select(any_of(c(aggregation,cols_to_keep,col_to_merge))) |>
                   distinct()                                               |>
                   group_by(across(c(aggregation,cols_to_keep)))            |>
                    summarise(across(any_of(col_to_merge), ~ str_flatten_comma(.x)),.groups="drop")

    data_sf <- suppressMessages(suppressWarnings(data_sf |>
              left_join(merged_col,by=c(aggregation,cols_to_keep)) |>
              relocate("geom",.after=last_col())
    ))
    }


   if(external_territories){

     data_sf_external <- suppressMessages(suppressWarnings(data_sf_external |>
                                                    group_by(across(c(aggregation,cols_to_keep))) |>
                                                    summarise(.groups="drop") |>
                                                    st_make_valid() |>
                                                    st_union(by_feature = TRUE) |>
                                                    fill_holes(set_units(smoothing_threshold,"km^2"))
     ))

     for(col_to_merge  in cols_to_merge){

       merged_col  <- filter_table |>
         select(any_of(c(aggregation,cols_to_keep,col_to_merge))) |>
         distinct()                                               |>
         group_by(across(c(aggregation,cols_to_keep)))            |>
         summarise(across(any_of(col_to_merge), ~ str_flatten_comma(.x)),.groups="drop")

       data_sf_external <- suppressMessages(suppressWarnings(data_sf_external |>
         left_join(merged_col,by=c(aggregation,cols_to_keep)) |>
         relocate(.data$geom,.after=last_col())
       ))
     }


      data_sf <- bind_rows(data_sf,data_sf_external)
   }




    join_key <- as.vector("geo_col")
    names(join_key) <- aggregation

    data_sf <- data_sf |>
               left_join(areas_prop,by=join_key) |>
                filter(if_any(c("prop"), ~  !is.na(.x)))


    exists_name <- any(colnames(data_sf)==str_replace(aggregation,"_(.*?)CODE","_NAME"))

    if(exists_name){
      agg_label <- colnames(data_sf)[str_detect(colnames(data_sf),
                                              str_replace(aggregation,"_(.*?)CODE","_NAME"))]

      data_sf$label <- data_sf |>
                      select(any_of(as.vector(agg_label))) |>
                      st_drop_geometry() |>
                       pull()

      data_sf <- data_sf |>
                  mutate(label=if_else(.data$prop==1,.data$label,str_c(.data$label, " (partial)")))

    }

  #simplify
    tryCatch(data_sf <- suppressMessages(suppressWarnings(
               data_sf |>
               ms_simplify(keep=simplification_factor))),
                error = function(e) e)

  }

  return(data_sf)

}
