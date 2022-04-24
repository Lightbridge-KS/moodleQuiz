#' Check Types of Moodle Quiz Report
#'
#' Check types of [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports)
#' whether it is Grades or Responses report.
#'
#' @param data A data.frame to test.
#'
#' @return Character vector: "Grades" for Moodle Grades Report, "Responses" for Moodle Responses Report,
#'  or `NA` if it's not a Moodle Quiz report
#'
#' @export
#' @examples
#' # Grades Report
#' get_report_type(grades_ls$Quiz_1)
#' # Responses Report
#' get_report_type(responses_ls$Quiz_1)
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

