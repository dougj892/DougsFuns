#' Generate dataframe of weighted mean of several variables.
#' Placeholder -- change so that the wt argument is optional.
#'
#' @param data Dataframe with columns to calculate mean of
#' @param vars <tidy-select> Columns to calculate means of
#' @param wt Variable for observation weights
#'
#' @return dataframe
#' @export
#'
#' @examples
#' tabstatr(mtcars, where(is.numeric), wt)
tabstatr <- function(data, vars, wt) {
  var_names <- rlang::enquo(vars)
  summary_row <- dplyr::summarize(data, dplyr::across(!!var_names, stats::weighted.mean, {{wt}}, na.rm = TRUE))
  summary_table <- tibble::rownames_to_column(as.data.frame(t(summary_row)), "variable")
  var_locs <- tidyselect::eval_select(var_names, data)
  var_labels <- purrr::map_chr(var_locs, ~as.character(attributes(data[[.x]])$label)[1] )
  summary_table$Label <- var_labels
  summary_table <- dplyr::relocate(summary_table, Label)
  summary_table <- dplyr::rename(summary_table, `Weighted mean` = V1)
  return(summary_table)
}

