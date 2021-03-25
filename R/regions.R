#' List all pre-defined regions
#' @return list of all regions
#' @import dplyr
#' @import tibble
#' @export list_regions
list_regions <- function(){
          load(system.file("extdata", "regions.rda", package = "aussiemaps"))
          regions %>% arrange(Region) %>% pull(.) %>% unique(.)

}

#' Obtain tibble with areas from a particular region - to be used with load_map()
#' @import dplyr
#' @import tibble
#' @param  region_name string with name of the region (use list_regions() to get all possible names)
#' @return tibble to
#' @export get_region
get_region  <- function(region_name){

              load(system.file("extdata", "regions.rda", package = "aussiemaps"))

              regions %>% filter(Region %in% region_name) %>% select(LGA_PID,State,Region) %>% unique(.)


}
