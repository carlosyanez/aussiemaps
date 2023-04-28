# Based on https://github.com/walkerke/aussiemaps/blob/master/R/helpers.R , released under MIT licence.
# Forked from {auspol}, with changes in downloading and importing files

#' Set the cache directory to store parquet files with aussiemaps
#'
#' @description By default, aussiemaps uses the rappdirs package to determine a suitable location to store shapefiles
#' on the user's computer.  However, it is possible that the user would want to store shapefiles in a custom
#' location.  This function allows users to set the cache directory, and stores the result in the user's
#' .Renviron so that aussiemaps will remember the location.
#'
#' Windows users: please note that you'll need to use double-backslashes or forward slashes
#' when specifying your cache directory's path in R.
#'
#' @param path The full path to the desired cache directory
#' @importFrom utils read.table write.table
#' @noRd
#' @examples \dontrun{
#' # Set the cache directory
#' cache_dir('PATH TO MY NEW CACHE DIRECTORY')
#'
#' # Check to see if it has been set correctly
#' Sys.getenv('aussiemaps_cache_dir')
#' }
manage_maps_cache_dir <- function(path) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if(!dir.exists(path)){
    dir.create(path,recursive = TRUE)
  }


  if (!file.exists(renv)) {
    file.create(renv)
  }

  check <- readLines(renv)

  if (isTRUE(any(grepl('aussiemaps_cache_dir', check)))) {
    oldenv <- read.table(renv,sep="=",stringsAsFactors = FALSE)
    newenv <- oldenv[!grepl('aussiemaps_cache_dir', oldenv$V1), ]
    write.table(newenv, renv, quote = FALSE, sep = "=",
                col.names = FALSE, row.names = FALSE)
  }

  var <- paste0("aussiemaps_cache_dir=", "'", path, "'")

  write(var, renv, sep = "\n", append = TRUE)
  message(sprintf("Your new cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", path))

}

#' Helper function to update/download  data
#' @importFrom fs dir_info
#' @param  ... fs::dir_info() parameters
#' @returns nothing
#' @export
#' @keywords helpers
data_maps_info <- function(...){
  cache_dir <- Sys.getenv('aussiemaps_cache_dir')
  dir_info(cache_dir,...)
}

#' Helper function to delete  data
#' @importFrom fs dir_ls file_delete
#' @returns nothing
#' @param  ... fs::dir_info() parameters
#' @export
#' @keywords helpers
data_maps_delete <- function(...){

  file <- data_maps_info(...)$path
  file_delete(file)
}

#' Helper function to find cache folder
#' @returns nothing
#' @export
#' @keywords helpers
find_maps_cache<- function(){
  cache_dir <- Sys.getenv('aussiemaps_cache_dir')
  return(cache_dir)

}

#' Get name for a cached map
#' @importFrom digest digest
#' @importFrom stringr str_c
#' @importFrom fs path
#' @param year year
#' @param simplification_factor simplification_factor
#' @param smoothing_threshold smoothing_threshold
#' @param new_crs new_crs
#' @param filter_table filter_table
#' @param aggregation aggregation
#' @returns cache path
#' @export
#' @keywords helpers
get_cache_name <-function(year,
                          simplification_factor,
                          smoothing_threshold,
                          new_crs,
                          filter_table,
                          aggregation) {
  hash <-
    str_c(year,
          simplification_factor,
          smoothing_threshold,
          new_crs,
          sep = "-")

  filter_table_hash <- digest(filter_table, "xxhash32", seed = 1234)
  aggregation_hash <- digest(aggregation, "xxhash32", seed = 1234)

  cache_file <- path(find_maps_cache(),
                     str_c("cache_", year, "_", digest(
                       str_c(hash, filter_table_hash, aggregation_hash, sep = "-"),
                       "xxhash32",
                       seed = 1234
                     )),
                     ext = "gpkg")

  return(cache_file)
}
