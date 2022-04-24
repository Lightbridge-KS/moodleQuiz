#' Get Type of Moodle Quiz Report
#'
#' @param data A data.frame to test.
#'
#' @return Character vector: `"Grades"` for Moodle Grades Report, `Responses`  for Moodle Responses Report,
#'  or `NA` if it's not a Moodle Quiz report
#'
#' @export
#'
get_report_type <- function(data) {

  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  is_gr <- is_grades_report(data)
  is_resp <- is_responses_report(data)
  out <- if(is_gr){
    "Grades"
  }else if(is_resp){
    "Responses"
  }else{
    NA_character_
  }
  out
}


#' Is it a Moodle Quiz Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Quiz Report
#' @noRd
is_report <- function(data) {

  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$moodle))
}


#' Is it a Moodle Grades Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Grades Report
#' @noRd
is_grades_report <- function(data) {

  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$grades))

}


#' Is it a Moodle Responses Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Responses Report
#' @noRd
is_responses_report <- function(data) {

  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$responses))

}

