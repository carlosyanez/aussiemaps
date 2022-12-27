#' Convert data, aggregating smallar geographic structures into larger ones.
#' @return tibble
#' @importFrom  dplyr select any_of collect rename left_join group_by across summarise anti_join bind_rows
#' @importFrom  rlang .data
#' @param original_data data frame with data
#' @param values_col name of the column with numeric data
#' @param original_geo name of the column representing the smallest geo unit (using the names from geo structure)
#' @param new_geo name of the column representing the largest geo unit (using the names from geo structure)
#' @param grouping_col vector with all other columns to be preserved during the transformation
#' @param year year the geo structures belong to
#' @param proportions_manual manual list of correspondences, it will override in-package proportions based on area.
#' @export
geo_aggregate <- function(original_data,
                          values_col,
                          original_geo,
                          new_geo,
                          grouping_col,
                          year,
                          proportions_manual=NULL){

  correspondence_table <- list_structure(year=year) |>
    select(any_of(c("id",original_geo,new_geo)))

  proportions_table <-   list_proportions(original_geo) |>
    collect()  |>
    select("id","geo_col","prop") |>
    rename(!!original_geo := "geo_col")


  correspondence_table <- correspondence_table |>
    left_join(proportions_table,by=c("id",original_geo)) |>
    group_by(across(c(original_geo,new_geo))) |>
    summarise(across(c("prop"), ~ sum(.x,na.rm=TRUE)),.groups = "drop")


  if(!is.null(proportions_manual)){

    proportions_prop <- colnames(proportions_manual)[!(colnames(proportions_manual) %in% c(original_geo,new_geo))]

    proportions_manual <- proportions_manual |>
      rename("prop"=proportions_prop)

    correspondence_table <- correspondence_table |>
      anti_join(proportions_manual,by=c(original_geo,new_geo)) |>
      bind_rows(proportions_manual)

  }


  year_num <- as.double(year)

  results <-       original_data |>
    rename(!!"Value":=any_of(c(values_col)))        |>
    left_join(correspondence_table,by=original_geo) |>
    select(-any_of(c(original_geo)))                |>
    group_by(across(c(new_geo,grouping_col)))        |>
    summarise(Value=sum(.data$Value*.data$prop,na.rm = TRUE),.groups = "drop") |>
    rename(!!values_col:="Value")

  return(results)

}