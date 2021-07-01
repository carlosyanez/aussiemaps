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
