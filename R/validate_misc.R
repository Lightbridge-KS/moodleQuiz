

# Is a multi-response colum ? -----------------------------------------------



#' Is a Column contained multiple answered MCQ
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (tidy-select) Column to test
#'
#' @return Named logical vector: `TRUE` if a column is a multiple answered MCQ
#' @noRd
is_multi_resp_col <- function(data, col) {

  col <- rlang::enquo(col)
  data %>%
    is_regex_in_cols(!!col, regex = "(\\r)*\\n;\\s*", match_rows = "any")

}
