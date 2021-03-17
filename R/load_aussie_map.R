#' Load granular data
#' @return no output
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import sf
#' @import tibble
#' @param filter_table table to filter (you can start with location_table)
#' @param aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @export load_map
load_map <- function(filter_table,aggregation=c("none")){

  ###auxiliary function
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }

     States  <- filter_table %>% select(State) %>%
                left_join(state_names,by="State") %>%
                mutate(State_new=if_else(!is.na(State_short),State_short,State)) %>%
                pull(State_new) %>% unique(.)

      data <- map_df(States,function(x){
      datai<- loadRData(str_c("inst/extdata/",tolower(x),"_lga_loc_poa.rda"))

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
        summarise(.groups = "drop")))

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
