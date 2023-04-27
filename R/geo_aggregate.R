#' Aggregate data to a new geography
#'
#' @description Convert data, aggregating smaller geographic structures into larger ones.
#' By default it uses area to apportion values when there is no one-to-one correspondence. Weighting table can be provided
#' @importFrom  dplyr select any_of collect rename left_join group_by across summarise anti_join bind_rows
#' @importFrom  rlang .data :=
#' @param original_data A data frame of original data
#' @param values_col The name of the column containing the values to be aggregated
#' @param original_geo The name of the column containing the original geography
#' @param new_geo The name of the column containing the new geography
#' @param grouping_col The name of the column containing the grouping variables
#' @param year The year of the data to be aggregated
#' @param proportions_manual A data frame of manual proportions,it will override in-package proportions based on area.
#' @return A data frame containing the aggregated data
#' @export
geo_aggregate <- function(original_data,
                          values_col,
                          original_geo,
                          new_geo,
                          grouping_col=NULL,
                          year,
                          proportions_manual=NULL){

  correspondence_table <- list_structure(year=year) |>
    select(any_of(unname(c("id",original_geo,new_geo))))

  proportions_table <-   list_proportions(unname(original_geo)) |>
                         collect()

  if(!any(str_detect(colnames(proportions_table),"prop"))){
    proportions_table$prop <- proportions_table$area/proportions_table$sum_area
  }

  proportions_table <- proportions_table |>
    select(any_of(c("id","geo_col","prop"))) |>
    rename(!!unname(original_geo) := "geo_col")


  correspondence_table <- correspondence_table |>
    left_join(proportions_table,by=unname(c("id",original_geo))) |>
    group_by(across(any_of(unname(c(original_geo,new_geo))))) |>
    summarise(across(any_of(c("prop")), ~ sum(.x,na.rm=TRUE)),.groups = "drop")


  if(!is.null(proportions_manual)){

    proportions_prop <- colnames(proportions_manual)[!(colnames(proportions_manual) %in% unname(c(original_geo,new_geo)))]

    proportions_manual <- proportions_manual |>
      rename("prop"=proportions_prop)

    correspondence_table <- correspondence_table |>
      anti_join(proportions_manual,by=unname(c(original_geo,new_geo))) |>
      bind_rows(proportions_manual)

  }


  year_num <- as.double(year)

  results <-       original_data |>
    rename(!!"Value":=any_of(c(values_col)))        |>
    left_join(correspondence_table,by=unname(original_geo),multiple="all") |>
    select(-any_of(unname(c(original_geo))))            |>
    group_by(across(any_of(unname(c(new_geo,grouping_col)))))     |>
    summarise(Value=sum(.data$Value*.data$prop,na.rm = TRUE),.groups = "drop") |>
    rename(!!values_col:="Value")

  return(results)

}
