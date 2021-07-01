#' Get list of all aggregation options
#' @return no output
#' @import dplyr
#' @import aussiemaps.data
#' @export list_agreggations
list_agreggations <- function(){

  colnames(aussiemaps.data::locations.table)

}
