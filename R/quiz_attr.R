
# Quiz Attributes ---------------------------------------------------------



#' Get Moodle Quiz Attributes
#'
#' Get attributes or meta-information from [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports) (i.e., Grades or Responses report)
#' such as type of Moodle Quiz report, maximum grade of each quiz, question's number, question's maximum score,
#' or embedded answer (Cloze) column names (if present).
#'
#' @param data A data.frame of [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports).
#'
#' @return A List containing Quiz attributes such as: \itemize{
#'   \item \strong{`report_type`}: (character) "Grades" for Moodle Grade report or "Responses" for Moodle Responses report.
#'   \item \strong{`some_nyg`}: (logical) `TRUE`: If "Grade/xx" column of the Moodle Quiz report contained some grades that are "Not yet graded".
#'   `FALSE`: If "Grade/xx" column contained all numeric grades.
#'   \item \strong{`grade_max`}: (numeric) Maximum grade of the Quiz
#'   \item \strong{`q_no`}: (numeric) Only if `report_type` is "Grades", then `q_no` shows questions number presented in the Moodle Grades report.
#'   \item \strong{`q_max`}: (numeric) Only if `report_type` is "Grades", then `q_max` shows maximum scores of corresponding `q_no`.
#'   \item \strong{`resp_no`}: (numeric)  Only if `report_type` is "Responses", then `resp_no` shows responses number of the Moodle Responses report.
#'   \item \strong{`cloze_cols`}: (character) Only if `report_type` is "Responses", then `cloze_cols` shows names of the "Responses" column that contained embedded answer (Cloze).
#'   If no Cloze column is presented, return `NULL`.
#' }
#'
#' @export
#'
#' @examples
get_quiz_attr <- function(data) {

  if(!is_report(data)) stop("`data` is not a moodle quiz report", call. = F)
  # Maximum Grade
  max_gr <- get_max_grade(data)
  some_nyg <- is_some_grade_nyg(data) # Is some student not yet graded?
  # If data is Grade Report, Get Questions Number and Max
  if(is_grades_report(data)){
    q_no_max <- get_questions_no_max(data)
    quiz_attr <- list(
      report_type = "Grades",
      some_nyg = some_nyg,
      grade_max = max_gr,
      q_no = q_no_max$q_no,
      q_max = q_no_max$q_max
    )
  }
  # If data is Responses Report, Get Responses Number
  if(is_responses_report(data)){
    resp_no <- get_responses_no(data)
    cloze_cols <- get_cloze_col_names(data)
    quiz_attr <- list(report_type = "Responses",
                      some_nyg = some_nyg,
                      grade_max = max_gr,
                      resp_no = resp_no,
                      cloze_cols = cloze_cols) # If NULL = no cloze column
  }

  quiz_attr

}


# Maximum Grade of Quiz ---------------------------------------------------



#' Get Maximum Grade of Quiz
#'
#' Get a maxium grade of any Moodle quiz report.
#'
#' @param df_raw A data.frame of Moodle Quiz report
#'
#' @return Numeric vector of length 1 indicate maximum quiz grade
#'
get_max_grade <- function(df_raw) {

  # Every moodle report has Maximum in Grade column name
  nm <- names(df_raw)
  gr_colnm <- stringr::str_subset(nm, "Grade")
  # Extract digits (including decimal)
  max_gr <- stringr::str_extract(gr_colnm, "[:digit:]+\\.?[:digit:]+$")
  as.numeric(max_gr)

}

# Maximum Responses of Responses Report ---------------------------------------------------


#' Get Maxiumn Responses of Moodle Responses report
#'
#' @param df_resp A data.frame of Moodle Responses report
#' @param count_cloze_parts (Logical) If `TRUE`, counts each parts of Cloze columns.
#'  If `FALSE`, count each Cloze column as 1.
#'
#' @return An Integer
#'
get_max_resp <- function(df_resp, count_cloze_parts = F) {

  # Length of all non-cloze response col
  noncloze_len <- length(get_noncloze_resp_colnm(df_resp))
  # Length of Cloze col
  cloze_len <- if(count_cloze_parts){
    # Sum of all cloze parts
    sum( get_cloze_attr(df_resp)$parts )
  }else{
    # Omit NA in case of `get_cloze_col_name` return NA if no cloze column
    get_cloze_col_names(df_resp) %>% length()
  }
  # Combine length
  tot_len <- noncloze_len + cloze_len
  tot_len
}



# Questions Number & Max --------------------------------------------------


#' Get Question Number and Maxiumn Score
#'
#' For Moodle Grades file, get a question number and corresponding question's maximum score.
#'
#' @param df_gr A data.frame of Moodle Grades report
#'
#' @return A data.frame with 2 columns: `q_no` for questions number, and `q_max` for maximum score.
#'
get_questions_no_max <- function(df_gr) {

  nm <- names(df_gr)
  q_colnm <- stringr::str_subset(nm, "Q")
  # Question No: Extract first set of digits before /
  q_number <- as.integer(stringr::str_extract(q_colnm, "[:digit:]+"))
  # Question Max: Extract everything after /
  q_max <- as.numeric(stringr::str_extract(q_colnm, "(?<=/)(.+)"))

  data.frame(q_no = q_number, q_max = q_max)
}


# Responses Numbers -------------------------------------------------------


#' Get Responses Number
#'
#' For Moodle Responses file, get a numeric vector for responses number.
#'
#' @param df_resp A data.frame of Moodle Response report
#'
#' @return Numeric vector
#'
get_responses_no <- function(df_resp) {

  nm <- names(df_resp)
  resp_colnm <- stringr::str_subset(nm, "R")
  resp_no <-  stringr::str_extract(resp_colnm, "[:digit:]+")
  as.integer(resp_no)

}
