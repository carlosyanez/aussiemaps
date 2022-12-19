#' Get list of all aggregation options
#' @return no output
#' @importFrom  dplyr mutate across select filter rename any_of if_any distinct bind_rows collect
#' @importFrom  stringr str_detect str_remove str_c
#' @importFrom  fs path
#' @importFrom  arrow read_parquet
#' @importFrom  tibble tibble
#' @export
list_attributes <- function(){

  cache_dir <- find_maps_cache()


  repo      <- read_parquet(path(cache_dir,"repo.parquet")) |>
               mutate(across(c("file_name"), ~ str_remove_all(.x,"\\.zip"))) |>
               select(any_of(c("file_name")))                                |>
               filter(if_any(c("file_name"), ~ str_detect(.x,"attributes")))

  #cache

  cache_files  <- data_maps_info() |>
                  filter(if_any(c("path"), ~ str_detect(.x,"attributes"))) |>
                  rename("file_name"="path") |>
                  select(any_of(c("file_name"))) |>
                  mutate(across(c("file_name"), ~ str_remove(.x,str_c(cache_dir,"/")))) |>
                  mutate(across(c("file_name"), ~ str_remove(.x,"\\.parquet")))

  attributes <- bind_rows(repo, cache_files) |>
                distinct()

  data <- tibble()

  for(file_name in attributes$file_name){

    data_i <- load_aussiemaps_parquet(file_name) |>
               collect()

    data_i$attr <- data_i$attributes

    data_i <- data_i |>
              mutate(across(c("attributes"), ~ str_remove(.x, "_[0-9]{4}")))

    data   <- bind_rows(data,data_i)

  }

  data <- data |>
          pivot_wider(names_from="Year",values_from = "attr") |>
          arrange("attributes")

  return(data)

}


#' Get list of all aggregation options
#' @return tibble with structure
#' @importFrom stringr str_c str_detect
#' @importFrom dplyr filter if_any collect mutate across
#' @export
list_structure <- function(year,filters=NULL){

  file_name <- str_c(year,"_structure")

  data <- load_aussiemaps_parquet(file_name)

  if(!is.null(filters)){
    for(i in 1:length(filters)){

      attr_i   <- names(filters)[i]
      values_i <- filters[[i]]

      data <- data |>
              filter(if_any(c(attr_i), ~ .x %in% values_i))

    }
  }

  data <- data |> collect()

  first_id <- data[1,]$id
  is_composite <- str_detect(first_id,"-")

  if(is_composite){
    data <- data |>
            separate("id",c("state","id"))
  }

  return(data)


}
