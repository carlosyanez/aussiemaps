#' Load sf polygons from selected locations
#' @return no output
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import sf
#' @import methods
#' @import tibble
#' @import lwgeom
#' @import rmapshaper
#' @import aussiemaps.data
#' @param filter_table table to filter (you can start with location_table)
#' @param aggregation name of column to aggregate (POA_CODE16, LOCALITY,LGA)
#' @param  clean_tolerance clean up tolerance
#' @param  simplify   whether to simplify polygons - TRUE (default) or FALSE
load_map <- function(filter_table,aggregation=c("none"), clean_tolerance=0.05,simplify=TRUE){

    #locally bind variables for RMD-check compatibility
    State       <- NULL
    State_short <- NULL
    State_new   <- NULL
    geometry    <- NULL

    States  <- filter_table %>% select(State) %>%
                left_join(aussiemaps.data::state.names,by="State") %>%
                mutate(State_new=if_else(!is.na(State_short),State_short,State)) %>%
                distinct(State_new)

    data <- tibble()

    #get shapes from each state/territory into data
    for(i in 1:nrow(States)){
      state <- States[i,1]
      datai<- aussiemaps.data::loadsfdata(state)

      data_cols <- colnames(as.data.frame(datai) %>% select(-State,-geometry))
      cols_filter <- colnames(filter_table %>% select(any_of(data_cols)))


      if(length(cols_filter)>0){
        datai <- suppressMessages(suppressWarnings(datai %>% inner_join(filter_table %>% select(-State), by=cols_filter)))

      }
      if(nrow(data)==0){
        data <-datai
      }else{
        data <- bind_rows(data,datai)
      }
    }

    #coerce into c()

    aggregation <- c(aggregation)
    if(!(aggregation[1]=="none")){
      sf::sf_use_s2(FALSE)
      data <- suppressMessages(suppressWarnings(data %>%
        group_by(across(starts_with(aggregation))) %>%
        summarise(.groups = "drop") %>%
        clean_polygons(clean_tolerance)))

    }

    #simplify
    if(simplify){
      data <- rmapshaper::ms_simplify(input = methods::as(data, 'Spatial')) %>%
              sf::st_as_sf()
    }


return(data)

}

