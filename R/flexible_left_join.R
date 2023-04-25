#' 'Flexible left join, accounting for different formatting in different datasets
#'
#' @description This function's preforms for a left_join() between to datasets, accomdating for small differences in
#' formatting across the key, for example a O'Connor vs. Oconnor (as in the Australian electoral division)
#' @importFrom  dplyr select any_of mutate across left_join
#' @importFrom stringr str_c str_replace_all str_to_lower str_squish
#' @param df1  first dataset (of sf data frame)
#' @param df2  second dataset
#' @param by named vector with join key, as in left_join
#' @return A data frame containing the aggregated data
#' @export
flexible_left_join <- function(df1,df2,by){
  ## uniform text
  col1 <- names(by)
  if(is.null(col1)){
    col1 <- by
  }

  df1 <- df1 |>
    mutate(across(any_of(str_c(col1,"_transf")), ~ str_replace_all(.x,"\\([^(,)]*\\)",""))) |>
    mutate(across(any_of(col1), ~ str_to_lower(.x),.names="{.col}_transf")) |>
    mutate(across(any_of(str_c(col1,"_transf")), ~ str_squish(.x))) |>
    mutate(across(any_of(str_c(col1,"_transf")), ~ str_replace_all(.x,"[^a-zA-Z0-9]","")))



  col2 <- by
  df2 <- df2 |>
    mutate(across(any_of(col2), ~ str_replace_all(.x,"\\([^(,)]*\\)",""))) |>
    mutate(across(any_of(col2), ~ str_to_lower(.x))) |>
    mutate(across(any_of(col2), ~ str_squish(.x)))   |>
    mutate(across(any_of(col2), ~ str_replace_all(.x,"[^a-zA-Z0-9]","")))


  names(col2) <- str_c(col1,"_transf")

  joined <- df1 |>
            left_join(df2,by=col2) |>
            select(-any_of(names(col2)))

  return(joined)

}
