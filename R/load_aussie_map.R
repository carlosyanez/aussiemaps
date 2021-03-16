#' Load granular data
#' @return no output
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import sf
#' @import tibble
#' @param filter_table table to filter (you can start with location_table)
#' @param aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @export load_aussie_map
load_aussie_map <- function(filter_table,aggregation=c("none")){

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

      cols_filter <- filter_table %>% select(-State)
      cols <- colnames(cols_filter)

      if(length(cols)>0){

        cols_filter <- cols_filter %>% mutate(check=TRUE)
        datai <- suppressMessages(datai %>% inner_join(cols_filter, by=cols) %>%
                                            select(-check))
      }

      datai

    })

    if(!(aggregation[1]=="none")){

      data <- suppressWarnings(data %>%
        group_by_at(aggregation) %>%
        summarise(.groups = "drop"))

    }

return(data)

}
