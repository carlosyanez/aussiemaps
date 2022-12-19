#' Get sf object containing selected map polygons
#' @return sf object with selected polygons
#' @importFrom arrow read_parquet
#' @importFrom dplyr mutate across select any_of filter if_any pull
#' @importFrom stringr str_remove_all str_detect str_c
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_as_sf st_as_sf
#' @param  filter_table table to filter (you can start with location_table)
#' @param  filters list with filters
#' @param  year year
#' @param  aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  clean_tolerance clean up tolerance
#' @param  simplify   (logical) whether to simplify polygons - True by default
#' @export
get_map <- function(filter_table=NULL,
                    filters=NULL,
                    year,
                    aggregation=NULL,
                    simplify=FALSE){

  if(is.null(filter_table)&is.null(filters)) stop("Either filter table or filters need to be provided")

  #get filter table if not provided
  if(is.null(filter_table)){
    filter_table <- list_structure(year,filters)
  }

  cache_dir  <- find_maps_cache()

  file_regex <- str_c(year,"_[A-Z]{1}")

  repo       <- read_parquet(path(cache_dir,"repo.parquet")) |>
                mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
                select(any_of("file_name"))                                   |>
                filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
                pull()


  state_col <- colnames(filter_table)[str_detect(colnames(filter_table),"STATE|STE")]
  state_col <- state_col[str_detect(state_col,"NAME")]

  required_states <- filter_table |>
                     select(all_of(c(state_col))) |>
                     distinct() |>
                     mutate(across(c(state_col), ~ str_replace_all(.x," ","\\."))) |>
                     mutate(across(c(state_col), ~ str_c(year,"_",.x))) |>
                     filter(if_any(c(state_col), ~ .x %in% repo))           |>
                     pull()

  data_sf <- NULL

  for(repo_i in required_states){
    data_i <- load_aussiemaps_gpkg(repo_i,filter_table)
    data_sf <- bind_rows(data_sf,data_i)

  }

  rm(data_i)


  #aggregate

  aggregation <- as.vector(aggregation)
  if(!(aggregation[1]=="none")){
    sf_use_s2(FALSE)
    data <- suppressMessages(suppressWarnings(data %>%
                                                group_by(across(starts_with(aggregation))) |>
                                                summarise(.groups = "drop")

  }

  #simplify
  if(simplify){
    data_sf <- ms_simplify(data_sf) |>
               st_as_sf()
  }


  return(data_sf)

}
