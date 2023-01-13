#############################################################
### Internal functions ####
#############################################################



## Based on https://github.com/walkerke/aussiemaps/blob/master/R/helpers.R , released under MIT licence.


#' Helper function to download data from github release
#'
#' @importFrom  piggyback pb_download_url
#' @importFrom  arrow open_dataset
#' @importFrom  stringr str_remove str_c str_detect
#' @importFrom utils download.file
#' @importFrom  zip unzip
#' @importFrom fs path
#' @param aussiemaps_file name of the file to download.
#' @return data frame or parquet binding
#' @noRd
load_aussiemaps <- function(aussiemaps_file) {

  cache_files <- data_maps_info()$path
  cache_dir   <- find_maps_cache()

  file_detect <- any(str_detect(cache_files,aussiemaps_file))

  if(!file_detect) {
    filename <- str_c(aussiemaps_file,".zip")

    url  <- pb_download_url(filename,
                              repo = "carlosyanez/aussiemaps",
                              tag = "data")

    file_path <- path(cache_dir,filename)

    download.file(url,path(cache_dir,filename))

    unzip(file_path,exdir = cache_dir)
    file.remove(file_path)
  }
  cache_files <- data_maps_info()$path
  file_path <- cache_files[str_detect(cache_files,aussiemaps_file)]


  return(file_path)
}

#' Helper function to import gpkg data
#'
#' @importFrom arrow open_dataset
#' @param aussiemaps_file name of the file to download.
#' @return sf parque binding
#' @noRd
load_aussiemaps_parquet <- function(aussiemaps_file){


  file_name <- load_aussiemaps(aussiemaps_file)
  data <- open_dataset(file_name,format="parquet")
  return(data)

}


#' Helper function to import gpkg data
#'
#' @importFrom fs path file_copy
#' @importFrom sf st_write st_read st_layers
#' @importFrom stringr str_c str_remove_all str_squish
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @param aussiemaps_file name of the file to download.
#' @param filter_ids data frame with ids to filter (id column)
#' @return sf data frame
#' @noRd
load_aussiemaps_gpkg <- function(aussiemaps_file,filter_ids=NULL){

  file_name <- load_aussiemaps(aussiemaps_file)
  temp_gpkg <- path(find_maps_cache(),"temp.gpkg")

  file_copy(file_name,temp_gpkg,overwrite=TRUE)

  data_layer <- st_layers(file_name)$name[1]

  if(!is.null(filter_ids)){
    st_write(filter_ids,temp_gpkg,layer="id",append=TRUE)
    query_text <- str_c("SELECT * FROM '",data_layer,"' WHERE id IN (SELECT id FROM id)")
  }else{
    query_text <- str_c("SELECT * FROM '",data_layer,"'")
  }

  data <- st_read(temp_gpkg,query=query_text) |>
    mutate(across(where(is.character), ~ str_squish(.x))) |>
    mutate(across(where(is.character), ~ str_remove_all(.x, "[^A-z|0-9|[:punct:]|\\s]")))

  return(data)
}

#' Update list of files in repo
#' @importFrom piggyback pb_list
#' @importFrom arrow write_parquet
#' @importFrom fs path file_exists file_info
#' @importFrom lubridate now interval days
#' @noRd
get_repo_files <- function(){

  cache_dir <-  find_maps_cache()
  local_repo <- path(cache_dir,"repo.parquet")

  if(file_exists(local_repo)){
    creation <- file_info(local_repo)$birth_time
    now <- now()

    age <- interval(creation,now)/days(1)
    if(age>1){
      repo      <- pb_list("carlosyanez/aussiemaps")
      write_parquet(repo,path(cache_dir,"repo.parquet"))
    }else{
      repo <- read_parquet(path(cache_dir,"repo.parquet"))
    }
  }else{
    repo      <- pb_list("carlosyanez/aussiemaps")
    write_parquet(repo,path(cache_dir,"repo.parquet"))
  }

  return(repo)

}



