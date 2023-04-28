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
#' @param new_crs CRS value if transformation is needed.
#' @param fill_holes whether to fill holes after merging parts
#' @param smoothing_threshold A number indicating the smoothing threshold.
#' @param use_cache A boolean indicating whether to use the cache.
#' @param cache_file Optional a string indicating the friendly name of the cache file (f not provided, an arbitrary name will be created).
#' @param cache_intermediates  whether to cache state intermediate aggregations
#' @param message_string extra message string to add to any message (for tracking)
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
                    simplification_factor=NULL, #simplification factor is a number
                    new_crs = NULL,
                    fill_holes=TRUE,
                    smoothing_threshold=4, #smoothing threshold is a number
                    use_cache=FALSE, #use cache is a boolean
                    cache_file=NULL,
                    cache_intermediates=TRUE,
                    message_string = ""
                    ){ #cache file is a string


  dummy <- lwgeom_extSoftVersion()
  if(is.null(filter_table)&is.null(filters)) stop("Either filter table or filters need to be provided")

  #get filter table if not provided
  if(is.null(filter_table)){
    filter_table <- list_structure(year,filters)
  }

  #create hash

  if(is.null(cache_file)){
    cache_file <- get_cache_name(year,simplification_factor,smoothing_threshold,new_crs,filter_table, aggregation)
  }else{

    cache_file <- cache_file
  }



  if(file_exists(cache_file) & use_cache){
    message(str_c(message_string,":: loading from cache: ", cache_file))
    data <- st_read(cache_file)

  }else{

    data <- get_map_internal(filter_table,
                             year,
                             aggregation,
                             simplification_factor,
                             new_crs,
                             fill_holes,
                             smoothing_threshold,
                             cache_intermediates,
                             message_string)
    if(use_cache){
      st_write(data,cache_file)
    }

  }

  return(data)
}



#' Get sf object containing selected map polygons - Internal function
#' @return sf object with selected polygons
#' @importFrom arrow read_parquet
#' @importFrom dplyr mutate reframe across select any_of filter if_any pull group_by starts_with left_join  if_else n everything matches relocate last_col contains matches
#' @importFrom stringr str_remove_all str_detect str_c str_extract str_replace str_replace_all
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_as_sf st_union st_make_valid sf_use_s2 st_drop_geometry st_buffer st_is_empty st_crs st_transform
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom units set_units
#' @importFrom smoothr fill_holes
#' @importFrom tidyselect where
#' @importFrom tibble tibble add_column
#' @importFrom nngeo st_remove_holes
#' @importFrom progressr  with_progress
#' @param  filter_table table to filter (you can start with location_table)
#' @param  filters list with filters
#' @param  year year
#' @param  aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  simplification_factor  0-1 simplication threshold to pass to rmapshaper::ms_simplify()
#' @param  new_crs use to transform projection
#' @param fill_holes whether to fill holes after merging parts
#' @param  smoothing_threshold smoothing threshold (default to 1, as in km^2)
#' @param  cache_intermediates whether to cache state intermediates
#' @noRd
get_map_internal <- function(filter_table=NULL,
                    year,
                    aggregation=NULL,
                    simplification_factor=NULL,
                    new_crs = NULL,
                    fill_holes = TRUE,
                    smoothing_threshold=4,
                    cache_intermediates=TRUE,
                    message_string=""){

  cache_dir  <- find_maps_cache()

  #aggregation - adding codes

  aggr_names <- aggregation[str_detect(aggregation,"NAME")]
  aggr_codes <- str_replace_all(aggr_names,"NAME","CODE")
  aggregation <- unique(c(aggregation,aggr_codes))

  #just in case, delete any zip files from cache (from aborted reads)
  zip_files <- dir_ls(cache_dir,regexp = "zip$")
  file_delete(zip_files)

  # continue

  file_regex <- str_c(year,"_[A-Z]{1}")

  repo_base <- get_repo_files() |>
                mutate(across(any_of(c("file_name")), ~ str_remove_all(.x,"\\.zip"))) |>
                select(any_of("file_name"))

  repo      <- repo_base |>
                filter(if_any(c("file_name"), ~ str_detect(.x,file_regex)))   |>
                mutate(across(any_of(c("file_name")), ~ str_remove_all(.x,"\\.[0-9]$")))     |>
                distinct() |>
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
  message(str_c(message_string,":: collecting"))
  for(repo_i in required_states){

    state_message <- str_c(message_string,":: ",repo_i," (",which(repo_i==required_states),"/",length(required_states),")")
    message(state_message)

    filter_table_hash <- digest(filter_table,"xxhash32",seed=1234)
    aggregation_hash <- digest(aggregation,"xxhash32",seed=1234)

    interm_cache_file <- path(find_maps_cache(),
                       str_c("intermediate_",year,"_",repo_i,"_",digest(str_c(repo_i,filter_table_hash,aggregation_hash,sep="-"),
                                                   "xxhash32",seed=1234)),
                       ext="gpkg")

    if(file_exists(interm_cache_file) & cache_intermediates){
      message(str_c(state_message,":: reading from intermediate cache"))
      data_i <- st_read(interm_cache_file,quiet=TRUE)

    }else{
      message(str_c(state_message,":: normalising"))

      data_base <- suppressMessages(suppressWarnings(load_aussiemaps_gpkg(repo_i,filter_table)))
      data_base <- data_base |>
        mutate(Year=year) |>
        mutate(across(where(is.character), ~str_squish(.x))) |>
        mutate(across(where(is.character), ~ str_remove_all(.x, "[^A-z|0-9|[:punct:]|\\s]"))) |>
        mutate(across(any_of(c("id")), as.character)) |>
        st_make_valid()

      cols_i <- colnames(data_base)
      cols_struct <- colnames(filter_table)


      missing_cols <- cols_struct[!(cols_struct %in% cols_i)]

      for(col in missing_cols){
        data_base <- data_base |> mutate(!!col := "NA")
      }


      message(str_c(state_message,":: merging"))
      with_progress(data_i <- map_merger(data_base,unique(c(aggregation,cols_to_keep))))

      #remove holes
      if(fill_holes & !is.null(smoothing_threshold)){
        message(str_c(message_string,":: filling holes"))
        data_i <- data_i |> st_make_valid()
        tryCatch(
          data_i <- fill_holes(data_i,set_units(smoothing_threshold,"km^2")),
          error = function(e) e)

        tryCatch(
          data_i <- st_remove_holes(data_i),
          error = function(e) e)
        data_i <- data_i |> st_make_valid()

        data_i <- data_resolver(data_i,aggregation,cols_to_keep,state_message)

      }



      st_write(data_i,interm_cache_file,append=FALSE,quiet=TRUE,delete_dsn=TRUE)

    }

    data_sf <- bind_rows(data_sf,data_i)

    rm(data_i)

  }


  #aggregate

  if(!is.null(aggregation)){
    message(str_c(message_string,":: aggregating by ",aggregation))

    aggregation <- as.vector(aggregation)
    aggreg_orig <- aggregation

    external_territories <- any(str_detect(required_states,"Other"))

    if(external_territories){
      data_sf_external  <- data_sf |> filter(if_any(any_of(as.vector(state_col)), ~ str_detect(.x,"Other")))
      data_sf           <- data_sf |> filter(if_any(any_of(as.vector(state_col)), ~ str_detect(.x,"Other",TRUE)))
    }
    #new aggregated sum
    areas_prop <- list()
    aggr_prop  <- aggregation[str_detect(aggregation,"CODE")]
    for(i in 1:length(aggr_prop)){
      areas_prop[[i]] <- load_aussiemaps_parquet(aggr_prop[i]) |>
                         filter(if_any(any_of(c("id")), ~ .x %in% filter_table$id)) |>
                         collect()

      if(!("prop" %in% colnames(areas_prop[[i]]))){
        areas_prop[[i]]$prop <- as.numeric(areas_prop[[i]]$area /  areas_prop[[i]]$sum_area)
      }
      areas_prop[[i]] <- areas_prop[[i]] |>
                        group_by(across(c("geo_col")))   |>
                         summarise(across(any_of("prop"), ~ sum(.x)),.groups="drop")


    }

    data_sf$empty <- st_is_empty(data_sf)

    data_sf <-  data_sf |>
                filter(if_any(any_of(c("empty")), ~ .x==FALSE))|>
                select(-any_of(c("empty")))

    merged_col  <- filter_table |>
                   mutate(Year=year) |>
                   select(any_of(c(aggregation,cols_to_merge))) |>
                   distinct()                                       |>
                   group_by(across(any_of(c(aggregation))))          |>
                   reframe(across(any_of(c(aggregation,cols_to_merge)), ~ merge_distinct(.x)))
    colnames(data_sf)
    colnames(merged_col)

    data_sf <- suppressMessages(suppressWarnings(data_sf |>
              left_join(merged_col,by=aggregation) |>
              relocate(any_of(c("geom","geometry")),.after=last_col())))

    if(external_territories){

     merged_col  <- filter_table |>
       mutate(Year=year) |>
       select(any_of(c(aggregation,cols_to_merge))) |>
       distinct()                                               |>
       group_by(across(any_of(c(aggregation))))            |>
       reframe(across(any_of(cols_to_merge), merge_distinct))

     data_sf <- suppressMessages(suppressWarnings(data_sf |>
                                                    left_join(merged_col,by=aggregation) |>
                                                    relocate(any_of(c("geom","geometry")),.after=last_col())))


      data_sf <- bind_rows(data_sf,data_sf_external)
   }

  #simplify
  if(!is.null(simplification_factor)){
    tryCatch(data_sf <- suppressMessages(suppressWarnings(
               data_sf |>
               ms_simplify(keep=simplification_factor))),
                error = function(e) e)
  }

  #change crs
    if(!is.null(new_crs)){
      tryCatch(data_sf <- suppressMessages(suppressWarnings(
        st_transform(data_sf,crs=st_crs(new_crs)))),
        error = function(e) e)
    }

  }

  data_sf <- data_sf |> select(-matches("\\.[0-9]$"))

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

#' @param df df
#' @param by_cols by_cols
#' @importFrom stringr str_c
#' @importFrom dplyr filter if_any left_join select bind_rows distinct mutate any_of row_number
#' @importFrom sf st_union st_as_sf st_drop_geometry
#' @importFrom tibble add_column
#' @importFrom progressr progressor
#' @description Internal function to create data_i, with progress
#' @noRd
map_merger <- function(df,by_cols){



  distinct_combos <- df |> st_drop_geometry() |>
    select(any_of(by_cols)) |>
    distinct() |>
    mutate(filter_flag=TRUE)

  x <-  1:nrow(distinct_combos)
  p <- progressr::progressor(along =x)
  data_i <- NULL

  for (j in seq_along(x)){
    #message(str_c(state_message,":: merging ",j," out of ",nrow(distinct_combos)))
    data_j <- suppressMessages(df |>
                                 select(-any_of(c("filter_flag"))) |>
                                 left_join(distinct_combos[j,],by=by_cols) |>
                                 filter(if_any(c("filter_flag"), ~ .x==TRUE)) |>
                                 select(-any_of(c("filter_flag"))))

    suppressWarnings(suppressMessages(data_j <- distinct_combos[j,] |>
      add_column(geom=st_union(data_j)) |>
      st_as_sf()))

    if(is.null(data_i)){
      data_i <- data_j
    }else{
      data_i <- bind_rows(data_i,data_j)
    }

    p(message = sprintf("%g", x[j]))
  }

  data_i <- data_i |>select(-any_of(c("filter_flag")))

  return(data_i)
}

#' @param df df
#' @param aggregation aggregation
#' @param cols_to_keep cols to keep
#' @param state_message state_message
#' @importFrom stringr str_c
#' @importFrom dplyr select any_of mutate  filter bind_rows anti_join
#' @importFrom sf st_cast st_make_valid st_difference st_covers st_area st_drop_geometry
#' @importFrom progressr with_progress
#' @description Internal function resolve overlaps
data_resolver <- function(df,aggregation,cols_to_keep,state_message){
  #df <- map
  suppressWarnings(suppressMessages(df_i <- df |> st_cast("POLYGON")))
  df_i <- df_i |> st_make_valid()
  #df$split_id <- 1:nrow(df_i)

  suppressWarnings(suppressMessages(diff_list <-  st_covers(df_i)))

  l <- c()
  for(i in 1:length(diff_list)){
    if(length(diff_list[[i]])>1) l <-c(l,i)

  }

  df_i$split_id <- 1:nrow(df_i)
  l <- df_i$split_id[l]

  if(!is.null(l)){
    message("Overlapping surfaces found")
    for(j in l){

    diff_orig <- df_i |> filter(if_any(any_of(c("split_id")), ~ .x==j))
    diff      <- diff_orig

    key_col <- diff |> st_drop_geometry()|> select(any_of(aggregation))  |> pull()

    #message(key_col)

    smaller_ids <-  diff_list[[j]][diff_list[[j]]!=j]
    small <- df_i |> filter(if_any(any_of(c("split_id")), ~ .x %in% smaller_ids))

    for(k in 1:nrow(small)){
      suppressWarnings(suppressMessages(diff <- st_difference(diff,small[k,])))
      diff <- st_make_valid(diff)
      diff$area <- st_area(diff)
      diff <- diff |>
        filter(if_any(any_of(c("area")), ~.x==max(area)))
      diff <- diff |> select(any_of(c(colnames(df),"split_id")))

    }

    suppressMessages(suppressWarnings(
    df_i <- df_i |>
      anti_join(diff_orig |> st_drop_geometry())  |>
      bind_rows(diff)
    ))
  }
    df_i <- df_i |> select(-any_of(c("split_id")))
  }

  message(str_c(state_message,":: merging after filling holes"))
  #with_progress(df_i <- map_merger(df_i,unique(c(aggregation,cols_to_keep))))
  return(df_i)

}


