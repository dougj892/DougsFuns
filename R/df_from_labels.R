

#' Generate dataframe of variable names and labels from Stata dataset
#'
#' This function takes a path to a Stata dataset and returns a dataframe containing
#' one column for the variable name and one column for the variable label.
#'
#' @param path A path to a Stata dataset.
#'
#' @return dataframe A dataframe with one column for the variable name and one column for the variable label.
#' @export
#'
#' @examples
#' df_from_labels("C:/Users/dougj/Documents/Data/APIS/APIS 2020/APIS 2020 HH.dta")
df_from_labels <- function(path){
  one_row <- haven::read_dta(path, n_max = 1)
  labels <- purrr::map_chr(one_row, ~as.character(attributes(.x)$label)[1])
  df <- data.frame(variable = names(one_row), label = labels)
}
