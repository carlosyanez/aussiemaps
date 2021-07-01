#' Update aussiemaps.data package
#' @return no output
#' @export update_maps
update_maps <- function(){
  devtools::install_github("carlosyanez/aussiemaps.data")
  message("maps upgraded")
}
