#' List of all Locality/Suburbs, Postal Area (POA) and Local Government Areas (LGA)
#' intersections present in sf structures.
#' You can use this dataset to build filter tables for load_map()
#'
#' @format A data frame with 9 variables:
#' \describe{
#'   \item{LOC_PID}{Unique ID for each locality/suburb (taken from data.gov.au)}
#'   \item{LGA_PID}{Unique ID for each local government area (taken from data.gov.au)}
#'   \item{LGA}{Name of local government area (e.g. Moreland)}
#'   \item{LOCALITY}{Name of locality or suburb (e.g. Brunswick)}
#'   \item{State.Region}{Name of "official" region within the state (e.g. Greater Metropolitan Melbourne))}
#'   \item{ABB_NAME}{Suburb name in uppercase}
#'   \item{Metro.Region}{Internal subdivision in Metro areas, if applies (.e.g Inner Metropolitan Melbourne)}
#'   \item{State}{State or Territory (.e.g Victoria)}
#'   \item{POA_CODE16}{Postal Area Code, as defined by ABS, (e.g. 3056)}
#' }
#' @source \url{https://data.gov.au/}
"locations.table"

#' List of states and their acronym
#'
#' @format A data frame with 9 variables:
#' \describe{
#'   \item{State_short}{2/3 letter acronym}
#'   \item{State}{Name}
#'   }
"state.names"
