#' Load granular data
#' @return no output
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import sf
#' @import tibble
#' @import lwgeom
#' @param filter_table table to filter (you can start with location_table)
#' @param aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  clean_tolerance clean up tolerance
#' @export load_map
load_map <- function(filter_table,aggregation=c("none"), clean_tolerance=0.05){

  ###auxiliary function
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }

     States  <- filter_table %>% select(State) %>%
                left_join(state.names,by="State") %>%
                mutate(State_new=if_else(!is.na(State_short),State_short,State)) %>%
                pull(State_new) %>% unique(.)

      data <- map_df(States,function(x){
      datai<- loadRData(system.file("extdata", str_c(tolower(x),"_lga_loc_poa.rda"), package = "aussiemaps"))

      data_cols <- colnames(as.data.frame(datai) %>% select(-State,-geometry))
      cols_filter <- colnames(filter_table %>% select(any_of(data_cols)))


      if(length(cols_filter)>0){
        datai <- suppressMessages(suppressWarnings(datai %>% inner_join(filter_table %>% select(-State), by=cols_filter)))
      }

      datai

    })

    if(!(aggregation[1]=="none")){

      data <- suppressMessages(suppressWarnings(data %>%
        group_by_at(aggregation) %>%
        summarise(.groups = "drop") %>%
        clean_polygons(clean_tolerance)))


    }

return(data)

}

#' Get list of all aggregation options
#' @return no output
#' @import dplyr
#' @export list_agreggations
list_agreggations <- function(){

    colnames(locations.table)

}

#' Clean up aggregated polygons
#' @return sf object
#' @import dplyr
#' @import sf
#' @import lwgeom
#' @param  sfobject sf object
#' @param  tol_value clean up tolerance
#' @export clean_polygons
clean_polygons <- function(sfobject,tol_value=0.05){

  col_names <- colnames(as.data.frame(sfobject) %>% select(-geometry))

  result <- suppressMessages(suppressWarnings(sfobject  %>%
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING") %>%
    st_collection_extract("LINESTRING") %>%
    st_polygonize() %>%
    st_collection_extract("POLYGON") %>%
    mutate(area=st_area(.)) %>%
    group_by_at(col_names) %>%
    mutate(tolerance=max(area)*tol_value) %>%
    filter(area>tolerance) %>%
    summarise(.groups = "drop") %>%
    st_collection_extract("POLYGON")))

   return(result)

}
