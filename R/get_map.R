#' Get a map sf tibble
#'
#' @description This function tibble with sf objects, for a particular year.
#' It allows to filter the results using a geo structure names / codes, and results can be aggregated by those too.
#' Optionally, this function stores the results in the cache for faster retrieval of large objects (e.g. when covering)
#' a metropolitan area.
#'
#'
#' @importFrom digest digest
#' @importFrom stringr str_c
#' @importFrom dplyr select matches
#' @importFrom sf st_read st_write
#' @importFrom lwgeom lwgeom_extSoftVersion
#' @param filter_table A data frame containing the filter table, usually the output of list_structure().
#' @param filters A list of filters to be used. Item names should name column names in list_structure(). Contents should be vectors with regular expressions.
#' @param year A number indicating the year for which the map should be created.
#' @param aggregation A vector containing the aggregation parameters,matching list_structure() column names .
#' @param simplification_factor A number indicating the simplification factor.
#' @param smoothing_threshold A number indicating the smoothing threshold.
#' @param use_cache A boolean indicating whether to use the cache.
#' @param cache_file Optional a string indicating the friendly name of the cache file (f not provided, an arbitrary name will be created).
#' @param cache_intermediates  whether to cache state intermediate aggregations
#'
#' @return A map object.
#'
#' @examples \dontrun{
#'  small case
#'  preston <- get_map(filters=list(SSC_NAME_2016=c("Preston")),
#'                       year=2016,
#'                      aggregation = c("SSC_NAME_2016"))
#'  big map of Sydney,cached
#'  sydney_area <- get_map(filter_table = greater_sydney,
#'                         year=2021,
#'                         aggregation = "GCCSA_NAME_2021",
#'                         use_cache = TRUE)
#'
#' }
#'
#' @export
#' @seealso \code{\link{list_structure}}
#'
get_map <- function(filter_table=NULL, #filter table is a data frame
                    filters=NULL, #filters is a list
                    year, #year is a number
                    aggregation=NULL, #aggregation is a list
                    simplification_factor=1, #simplification factor is a number
                    smoothing_threshold=4, #smoothing threshold is a number
                    use_cache=FALSE, #use cache is a boolean
                    cache_file=NULL,
                    cache_intermediates=TRUE){ #cache file is a string


  dummy <- lwgeom_extSoftVersion()
  if(is.null(filter_table)&is.null(filters)) stop("Either filter table or filters need to be provided")

  #get filter table if not provided
  if(is.null(filter_table)){
    filter_table <- list_structure(year,filters)
  }

  #create hash

  if(is.null(cache_file)){
    hash <- str_c(year,simplification_factor,smoothing_threshold,sep="-")

    filter_table_hash <- digest(filter_table,"xxhash32",seed=1234)
    aggregation_hash <- digest(aggregation,"xxhash32",seed=1234)

    cache_file <- path(find_maps_cache(),
                       str_c("cache_",year,"_",digest(str_c(hash,filter_table_hash,aggregation_hash,sep="-"),
                                             "xxhash32",seed=1234)),
                       ext="gpkg")
  }else{

    cache_file <- cache_file
  }



  if(file_exists(cache_file) & use_cache){
    message(str_c("loading from cache: ", cache_file))
    data <- st_read(cache_file)

  }else{

    data <- get_map_internal(filter_table,
                             year,
                             aggregation,
                             simplification_factor,
                             smoothing_threshold,
                             cache_intermediates)
    if(use_cache){
      st_write(data,cache_file)
    }

  }

  return(data)
}



#' Get sf object containing selected map polygons - Internal function
#' @return sf object with selected polygons
#' @importFrom arrow read_parquet
#' @importFrom dplyr mutate reframe across select any_of filter if_any pull group_by starts_with left_join  if_else n everything matches relocate last_col contains
#' @importFrom stringr str_remove_all str_detect str_c str_extract str_replace str_replace_all
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_as_sf st_union st_make_valid sf_use_s2 st_drop_geometry st_buffer st_is_empty
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom units set_units
#' @importFrom smoothr fill_holes
#' @importFrom tidyselect where
#' @importFrom tibble tibble
#' @param  filter_table table to filter (you can start with location_table)
#' @param  filters list with filters
#' @param  year year
#' @param  aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  simplification_factor  0-1 simplication threshold to pass to rmapshaper::ms_simplify()
#' @param  smoothing_threshold smoothing threshold (default to 1, as in km^2)
#' @param  cache_intermediates whether to cache state intermediates
#' @noRd
get_map_internal <- function(filter_table=NULL,
                    year,
                    aggregation=NULL,
                    simplification_factor=1,
                    smoothing_threshold=4,
                    cache_intermediates=TRUE){

  cache_dir  <- find_maps_cache()

  #just in case, delete any zip files from cache (from aborted reads)
  zip_files <- dir_ls(cache_dir,regexp = "zip$")
  file_delete(zip_files)

  # continue

  file_regex <- str_c(year,"_[A-Z]{1}")

  repo_base      <- get_repo_files() |>
                mutate(across(any_of(c("file_name")), ~ str_remove_all(.x,"\\.zip"))) |>
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

  cols_to_keep <- sort(unique(c(cols_to_keep,"Year")))
  cols_to_keep <- cols_to_keep[str_detect(cols_to_keep,"AREA_ALBERS_SQKM",TRUE)]

  #columns to merge

  cols_to_merge <- colnames(filter_table)
  cols_to_merge <- cols_to_merge[!(cols_to_merge %in% c(cols_to_keep,aggregation,"id","area"))]
  cols_to_merge <- cols_to_merge[str_detect(cols_to_merge,"AREA_ALBERS_SQKM",TRUE)]

  data_sf <- NULL
  message("collecting")
  for(repo_i in required_states){
    message(repo_i)
    filter_table_hash <- digest(filter_table,"xxhash32",seed=1234)
    aggregation_hash <- digest(aggregation,"xxhash32",seed=1234)

    interm_cache_file <- path(find_maps_cache(),
                       str_c("intermediate_",year,"_",digest(str_c(repo_i,filter_table_hash,aggregation_hash,sep="-"),
                                                   "xxhash32",seed=1234)),
                       ext="gpkg")

    if(file_exists(interm_cache_file) & cache_intermediates){
      message("reading from intermediate cache")
      data_i <- st_read(interm_cache_file,quiet=TRUE)

    }else{

      data_base <- suppressMessages(suppressWarnings(load_aussiemaps_gpkg(repo_i,filter_table)))
      message("normalising")
      data_base <- data_base |>
        mutate(Year=year) |>
        mutate(across(where(is.character), ~str_squish(.x))) |>
        mutate(across(where(is.character), ~ str_remove_all(.x, "[^A-z|0-9|[:punct:]|\\s]"))) |>
        mutate(across(any_of(c("id")), as.character)) |>
        st_make_valid()

      #col_names <- colnames(data_base[1,])

      #distinct values
      distinct_combos <- data_base |> st_drop_geometry() |>
                        select(any_of(unique(c(aggregation,cols_to_keep)))) |>
                        distinct() |>
                        mutate(filter_flag=TRUE)
      data_i <- NULL

      for(j in 1:nrow(distinct_combos)){
          message(str_c("merging ",j," out of ",nrow(distinct_combos)))
          data_j <- suppressMessages(data_base |>
                    left_join(distinct_combos[j,],by=c(aggregation,cols_to_keep)) |>
                    filter(filter_flag) |>
                    select(-any_of(c("filter_flag"))) |>
                    group_by(across(any_of(c(aggregation,cols_to_keep)))) |>
                    summarise(.groups="drop") |>
                    st_make_valid())
          if(is.null(data_i)){
            data_i <- data_j
          }else{
            data_i <- bind_rows(data_i,data_j)
          }
      }

      st_write(data_i,interm_cache_file,append=FALSE,quiet=TRUE,delete_dsn=TRUE)

    }

    data_sf <- bind_rows(data_sf,data_i)
    rm(data_i)

  }

  #aggregate

  if(!is.null(aggregation)){
    message(str_c("aggregating by ",aggregation))

    aggregation <- as.vector(aggregation)
    aggreg_orig <- aggregation


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
      data_sf           <- data_sf |> filter(if_any(as.vector(state_col), ~ str_detect(.x,"Other",TRUE)))
    }
    #new aggregated sum
    areas_prop <- list()
    for(i in 1:length(aggregation)){
      areas_prop[[i]] <- load_aussiemaps_parquet(aggregation[i]) |>
                         filter(if_any(c("id"), ~ .x %in% filter_table$id)) |>
                         collect() |>
                         group_by(across(c("geo_col")))   |>
                         summarise(across(any_of("prop"), ~ sum(.x)),.groups="drop")


    }
    #sf_use_s2(FALSE)
    data_sf <- suppressMessages(suppressWarnings(data_sf |>
                                                st_make_valid() |>
                                                st_buffer(0) |>
                                                group_by(across(any_of(unique(c(aggreg_orig,
                                                                         aggregation,
                                                                         cols_to_keep))))) |>
                                                summarise(.groups="drop") |>
                                                st_make_valid() |>
                                                st_union(by_feature = TRUE) |>
                                                st_make_valid()))

    data_sf$empty <- st_is_empty(data_sf)

    data_sf <-  data_sf |>
                filter(!empty)|>
                select(-any_of(c("empty"))) |>
                fill_holes(set_units(smoothing_threshold,"km^2"))

    merged_col  <- filter_table |>
                   mutate(Year=year) |>
                   select(any_of(c(aggregation,cols_to_merge))) |>
                   distinct()                                       |>
                   group_by(across(any_of(c(aggregation))))          |>
                   reframe(across(any_of(cols_to_merge), ~ merge_distinct(.x)))

    data_sf <- suppressMessages(suppressWarnings(data_sf |>
              left_join(merged_col,by=aggregation) |>
              relocate("geom",.after=last_col())))

   if(external_territories){

     data_sf_external <- suppressMessages(suppressWarnings(data_sf_external |>
                                                    group_by(across(any_of(c(aggregation,cols_to_keep)))) |>
                                                    summarise(.groups="drop") |>
                                                    st_make_valid() |>
                                                    st_union(by_feature = TRUE) |>
                                                    fill_holes(set_units(smoothing_threshold,"km^2"))
     ))

     merged_col  <- filter_table |>
       mutate(Year=year) |>
       select(any_of(c(aggregation,cols_to_merge))) |>
       distinct()                                               |>
       group_by(across(any_of(c(aggregation))))            |>
       reframe(across(any_of(cols_to_merge), ~ merge_distinct(.x)))

     data_sf <- suppressMessages(suppressWarnings(data_sf |>
                                                    left_join(merged_col,by=aggregation) |>
                                                    relocate("geom",.after=last_col())))


      data_sf <- bind_rows(data_sf,data_sf_external)
   }

  #simplify
    tryCatch(data_sf <- suppressMessages(suppressWarnings(
               data_sf |>
               ms_simplify(keep=simplification_factor))),
                error = function(e) e)

  }

  data_sf <- data_sf |> st_make_valid()

  return(data_sf)

   }

#' @importFrom stringr str_flatten_comma
#' @param x vector of strings
#' @description auxiliary function to flatten a vector of strings into a comma separated string, sorting
#' alphabetically and removing duplicates
#' @noRd
merge_distinct <- function(x){
  x <- unique(x)
  x <- sort(x)
  str_flatten_comma(x)
}
