

# Cloze Parts --------------------------------------------------------


#' Count Individual Parts of Cloze
#'
#' @param data A data.frame of Moodle Responses report
#'
#' @return A data.frame with 2 columns: \strong{`cloze_colnm`} for Cloze column names, and \strong{`parts`} for number of parts for each Cloze answers.
#'
get_cloze_attr <- function(data) {

  if(!has_cloze_col(data)) stop("`data` has no cloze column.", call. = F)

  cloze_colnm <- get_cloze_col_names(data)
  cloze_parts <- purrr::map_int(cloze_colnm, ~get_cloze_parts(data, .x))

  df <- data.frame(cloze_colnm = cloze_colnm, parts = cloze_parts)
  df
}

#' Get Cloze parts for 1 Cloze column
#'
#' @param data A data.frame of Moodle Responses report
#' @param cloze_col (character) Specify Cloze column to count parts.
#'
#' @return Numeric: indicate total parts of single Cloze answer.
#'
get_cloze_parts <- function(data, cloze_col){

  cloze_regex <- "part [:digit:]+:"

  data[[cloze_col]] %>%
    stringr::str_count(cloze_regex) %>%
    unique() %>%
    max(na.rm = T)

}

# Cloze column names ------------------------------------------------------



#' Get Cloze column names
#'
#' @param df A data.frame of Moodle Responses report
#'
#' @return Character vector indicate Cloze column names. If no cloze column, return `NULL`.
#'
get_cloze_col_names <- function(df) {

  regex <- "part [:digit:]+:"
  is_cloze_lgl <- df %>%
    purrr::map(~stringr::str_detect(.x, regex)) %>%
    purrr::map_lgl(any)

  nm <- names(which(is_cloze_lgl))
  if(length(nm) == 0) return(NULL)
  nm
}


# Non-cloze column names --------------------------------------------------



#' Get Non-Cloze Responses column name
#'
#' @param df A data.frame of Moodle Responses report
#'
#' @return Character vector indicate Non-Cloze responses column names. If all responses are cloze column, return `NULL`.
#'
get_noncloze_resp_colnm <- function(df) {

  regex <- "part [:digit:]+:"
  is_cloze_lgl <- df %>%
    dplyr::select(tidyselect::starts_with("Response")) %>%
    purrr::map(~stringr::str_detect(.x, regex)) %>%
    purrr::map_lgl(any)

  nm <- names(which(!is_cloze_lgl))
  if(length(nm) == 0) return(NULL) #return(NA_character_)
  nm

}


# Is it a Cloze col -------------------------------------------------------


#' Is A Column contains Cloze Answers
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (tidy-select) column to test
#'
#' @return Logical: `TRUE` if `col` is a Cloze column.
#'
is_cloze_col <- function(data, col) {

  col <- rlang::enquo(col)
  data %>%
    is_regex_in_cols(!!col, regex = "part [:digit:]+:", match_rows = "any")

}


# Has cloze column --------------------------------------------------------


#' Has any Cloze column?
#'
#' @param df A data.frame of Moodle Responses report
#'
#' @return Logical: `TRUE` if `df` has at least one Cloze column.
#'
has_cloze_col <- function(df) {

  regex <- "part [:digit:]+:"
  is_cloze_lgl <- df %>%
    purrr::map(~stringr::str_detect(.x, regex)) %>%
    purrr::map_lgl(any)

  any(is_cloze_lgl, na.rm = T)
}
