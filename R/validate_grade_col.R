#' Is some Grade Numeric ?
#'
#' Check in Grade column of the Moodle Quiz report. Are there any Numeric Grade?
#'
#' @param data A data.frame to test.
#'
#' @return Logical: `TRUE` if numeric grade is presented.
#' @noRd
is_some_grade_numeric <- function(data){

  data %>%
    dplyr::select(tidyselect::starts_with("Grade")) %>%
    unique() %>%
    dplyr::pull() %>%
    # Detect start with one or more digit can be followed by dot and end with digit
    stringr::str_detect("^[:digit:]+\\.?[:digit:]+$") %>%
    any()

}


#' Is some Grade "Not yet graded"
#'
#' Check in Grade column of the Moodle Quiz report. Are there any "Not yet graded"?
#'
#' @param data A data.frame to test.
#'
#' @return Logical: `TRUE` if "Not yet graded" is presented.
#' @noRd
is_some_grade_nyg <- function(data){

  data %>%
    dplyr::select(tidyselect::starts_with("Grade")) %>%
    unique() %>%
    dplyr::pull() %>%
    # Detect any "Not yet graded"
    stringr::str_detect("Not yet graded") %>%
    any()

}
