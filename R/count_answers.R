### Count Answers from Response Columns


# Count Answers from all responses ----------------------------------------


#' Count Student's Answers
#'
#' [count_answers()] counts student's answers from every "Response" columns from from a [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports).
#' This is a wrapper around [count_answers_col()] that counts from one column.
#' Answers from Moodle's quiz can be one of 3 types: single answered, multiple answered, or [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type).
#' Each of these answers types formatted differently in the "Response" column.
#' [count_answers()] knows each type of answers and count the answers by the best counting method it thinks.
#'
#'
#' @param data A data.frame of [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report)
#' @param count_type A character indicate type of counting methods. Must be one of:
#'   * \strong{"auto"} (default): This function detects the type of questions automatically and choose the best counting method it thinks for each type.
#'   * \strong{"single"}: It will simply count each answers by assuming that the question is a single answer type.
#'     If the question is multiple answers type, counts of every combinations of the answers will be returned.
#'   * \strong{"multi"}: If the question is multiple answers type, this will give the count be each answers.
#'   * \strong{"cloze"}: If the question is embedded answers (Cloze), this will give the count by every "Parts" of the Cloze question pooled together.
#' @param sort (Logical) `TRUE`: sort the counted results
#' @param round_digits Integer to round the counted percentage or `NULL` to not round.
#'
#' @return A data.frame
#' @export
#'
count_answers <- function(data,
                          count_type = c("auto", "single", "multi", "cloze"),
                          sort = TRUE,
                          round_digits = 2
) {

  if(!is_responses_report(data)) stop("`data` is not a Moodle Responses report.", call. = F)

  resp_no <- get_responses_no(data)
  resp_cols <- names(data) %>% stringr::str_subset("R")
  # Automatic Count for each column
  resp_cols %>%
    stats::setNames(resp_no) %>%
    purrr::map(~count_answers_col(data, col = !!.x,
                                  count_type = count_type,
                                  count_cloze_parts = FALSE,
                                  sort = sort, round_digits = round_digits
    ))  %>%
    # Bind List to DF
    dplyr::bind_rows(.id = "Questions")  %>%
    tidyr::unite(col = "Answers", tidyselect::starts_with("R"), na.rm = T)
}

# Count Answers from Column -----------------------------------------------


#' Count Student's Answers from a Response Column
#'
#' Count student's answers from a given "Response" column from a [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports). This function will count depending on
#' type of question. If the question is a single answer type, it will simply count the answers.
#' If the question is multiple answers type, it will count by each answers (default) or by combinations of answers.
#' If the question is [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type),
#' it will count the answers by every "Parts" of the Cloze question pooled together (default), or by each "Parts" of the Cloze question.
#'
#' @param data A data.frame of [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report)
#' @param col Quoted or unquoted response column name that you want to count.
#' @param count_type A character indicate type of counting methods. Must be one of:
#'   * \strong{"auto"} (default): This function detects the type of question automatically and choose the best counting method it thinks.
#'   * \strong{"single"}: It will simply count each answers by assuming that the question is a single answer type.
#'     If the question is multiple answers type, counts of every combinations of the answers will be returned.
#'   * \strong{"multi"}: If the question is multiple answers type, this will give the count be each answers.
#'   * \strong{"cloze"}: If the question is embedded answers (Cloze), this will give the count by every "Parts" of the Cloze question pooled together (default).
#'   If `count_cloze_parts = TRUE`: it will count by each "Parts" of the Cloze question.
#' @param count_cloze_parts (Logical) Count each "Parts" of the Cloze question or not
#' @param sort (Logical) `TRUE`: sort the counted results
#' @param round_digits Integer to round the counted percentage or `NULL` to not round.
#'
#' @return A data.frame
#' @noRd
count_answers_col <- function(data, col,
                              count_type = c("auto", "single", "multi", "cloze"),
                              count_cloze_parts = FALSE, # Applies to only cloze col
                              sort = TRUE,
                              round_digits = 2
) {

  col <- rlang::ensym(col)
  count_type <- rlang::arg_match(count_type)
  is_multi <- is_multi_resp_col(data, !!col)
  is_cloze <- is_cloze_col(data, !!col)

  counted <- switch (count_type,
                     "auto" = {
                       if (is_cloze){ # Cloze cols
                         count_answers_cloze_col(data, !!col, count_cloze_parts = count_cloze_parts,
                                                 sort = sort)
                       }else if (is_multi){ # Multi Answer col
                         count_answers_multi_col(data, !!col, sort = sort)
                       }else{ # Single Answer col
                         count_answers_single_col(data, !!col, sort = sort)
                       }

                     },
                     "single" = {
                       count_answers_single_col(data, !!col, sort = sort)
                     },
                     "multi" = {
                       count_answers_multi_col(data, !!col, sort = sort)
                     },
                     "cloze" = {
                       count_answers_cloze_col(data, !!col, count_cloze_parts = count_cloze_parts,
                                               sort = sort)
                     }
  )
  if(is.null(round_digits)) return(counted)

  # Round Percent
  counted %>%
    dplyr::mutate(
      dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                    ~round(.x, digits = round_digits))
    )
}

# Count Single Answered MCQ -----------------------------------------------


#' Count Answers from Single Answered type question
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (quoted or unquoted) A column that contain Single Answered type question
#' @param sort (Logical) sorted or not
#'
#' @return A data.frame
#' @noRd
count_answers_single_col <- function(data,
                                     col,
                                     sort = TRUE) {

  col <- dplyr::ensym(col)
  data %>%
    dplyr::count(!!col, sort = sort) %>%
    dplyr::mutate(Percent = (n/sum(n))*100)
}

# Count Multi-Answered MCQ ------------------------------------------------


#' Count Answers from Multiple Answered type question
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (quoted or unquoted) A column that contain Multiple Answered type question
#' @param sort (Logical) sorted or not
#'
#' @return A data.frame
#' @noRd
count_answers_multi_col <- function(data,
                                    col,
                                    sort = TRUE
) {

  col <- dplyr::ensym(col)
  data %>%
    dplyr::count(!!col, sort = sort) %>%
    # Separate
    tidyr::separate_rows(!!col, sep = "(\\r)*\\n;\\s*") %>%
    # Second Count
    dplyr::count(!!col, wt = n, sort = sort) %>%
    # Percent
    dplyr::mutate(Percent = (n/sum(n))*100)

}

# Count Cloze Answers ------------------------------------------------------

#' Count Answers from Cloze column
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (quoted or unquoted) A column that contain Cloze answers
#' @param count_cloze_parts (Logical) If `TRUE`: count answers in each parts of Cloze column.
#' @param sort (Logical) sorted or not
#'
#' @return A data.frame
#' @noRd
count_answers_cloze_col <- function(data,
                                    col,
                                    count_cloze_parts = FALSE, # Count Each parts of cloze?
                                    sort = TRUE
){

  col <- rlang::ensym(col)

  # First Count before separate
  counted_1 <- data %>%
    dplyr::count(!!col) %>%
    split_cloze_col(!!col, split_type = "rows")
  # Grouping Expression
  group_exprs <- if(count_cloze_parts){
    rlang::exprs(
      dplyr::across(c(tidyselect::ends_with("parts"), tidyselect::ends_with("answers")))
    )
  }else{
    rlang::exprs(dplyr::across(tidyselect::ends_with("answers")))
  }
  # Second Count
  counted_2 <- counted_1 %>%
    dplyr::group_by(!!!group_exprs) %>%
    dplyr::summarise(n = sum(n), .groups = "drop_last") %>%
    # Percent
    dplyr::mutate(Percent = n/sum(n))

  if(sort){
    counted_2 <- counted_2 %>%
      dplyr::arrange(dplyr::desc(n), .by_group = TRUE)
  }
  counted_2 %>%
    dplyr::ungroup()

}
