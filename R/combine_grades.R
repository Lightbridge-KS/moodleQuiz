
# Generic: Combine Grades -------------------------------------------------



#' Combine Grades
#'
#' [combine_grades()] is a generic function that combine, adjust, and filter student grades from [Moodle Grades report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Responses report).
#' This function can combine "Grade/xx" columns from multiple Moodle Grades reports into a single data.frame.
#' Furthermore, you can adjust or weight maximum score of each quizzes and "Total" score will be computed.
#' If the report contains multiple attempts per student, you can also filter grades of each student by score ("Grade/xx" column) and start time ("Started on" column), e.g. the first best score of each students.
#' Like [check_sub()], this function also cleans column names for easy manipulation, extracts student ID from "Email address", and unites "First name" and "Surname" column into "Name".
#'
#' @param data A data.frame **or** named list of data.frame of [Moodle Grades report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Responses report)
#' @param id_regex (Character) A regular expression used to extract ID from column "Email address" in the Moodle Quiz report. The default is "`.*`" meaning all characters.
#'   If your student email addresses has numeric IDs in them, try "`[:digit:]+`" to extract digits from the email.
#'   **Note**: Regular expression syntax is the same as [stringr](https://github.com/rstudio/cheatsheets/blob/master/strings.pdf).
#' @param sep_name A character in the new "Name" column that separate original "First name" and "Surname".
#' @param new_max_grade (Numeric) A desired new maximum grade(s) to adjust. If `NULL` (default): the new maximum grade is/are equal to the old maximum grade from the Moodle Grades report (in "Grade/xx" columns).
#' **Note**: The new maximum grade will be appended at the "Grade_*" column names.
#' \itemize{
#'   \item If the `data` is a **data.frame**; enter `new_max_grade` as length 1 numeric vector to indicate the new maximum grade of that quiz.
#'   \item If the `data` is a **named list of data.frame**; enter `new_max_grade` as numeric vector to indicate new maximum grades corresponding to each quizzes (the element of list of data.frame).
#'     Length of `new_max_grade` must equal to length of list of data.frame.
#' }
#' @param round_digits Length 1 numeric vector to indicate digits to round grades. If `NULL`: no round digits.
#' @param choose_grade A character to filter student's attempt by score ("Grade/xx" column), useful when quiz has multiple attempts for each students.
#'   * \strong{"max"} (default): return rows that have maximum score of each students.
#'   * \strong{"min"}: return rows that have minimum score of each students.
#'   * \strong{"mean"}: compute and return mean score of each students.
#'   * \strong{"all"}: no filter applied, return all rows.
#' @param choose_time A character to filter student's attempt by started time (determined by "Started on" column).
#'   This filter applies **after** `choose_grade` has been applied to the data.
#'   * \strong{"first"} (default): return rows that represent first attempt of each students.
#'   * \strong{"last"}: return rows that represent last attempt of each students.
#'   * \strong{"all"}: no filter applied, return all rows after applying `choose_grade`
#' @param force_grade (Logical) If grade of any students was found to be "Not yet graded",
#'   you will get an error message. The purpose of this is to warn you that you might have forgotten to grade some students.
#'   If you want to bypass this behavior, set `force_grade = TRUE`.
#' @param sep_col (Character) If `data` is a named list of data.frame, `sep_col` indicate a character separation between names of list and "State" or "Grade" columns.
#'
#' @return **A data.frame**, its output content is determined by class of its first argument: `data`.
#'   * If the `data` is a data.frame; the output is a cleaned data.frame of Moodle Grades report, whether it was adjusted or filtered depends on other arguments.
#'   * If the `data` is a named list of data.frame; the output is the same as previously described, but all Moodle Grades reports are [full-joined](https://dplyr.tidyverse.org/reference/mutate-joins.html) together by column "Name" and "ID".
#'   So that, "Grades_" columns from each data.frame are sit together in a single data.frame, and "Total_" column is added at the last column to indicate sum of all quizzes grades.
#' @export
#'
#' @examples NULL
combine_grades <- function(data,
                           # Clean
                           id_regex = ".*",
                           sep_name = " ", # Separate First name and Surname
                           # Adjust Grade
                           new_max_grade = NULL,
                           ### NULL = no adjust,
                           ### (DF method): length 1 Numeric
                           ### (list method): Numeric vector same length as length `data`
                           round_digits = 3, # NULL = no round
                           # Filter Grades
                           choose_grade = c("max", "min", "mean", "all"),
                           choose_time = c("first", "last", "all"),
                           force_grade = F,
                           sep_col = "_" # Separation for State and Grade column names
) {

  UseMethod("combine_grades")

}


# List method -------------------------------------------------------------

#' @export
combine_grades.list <- function(data,
                                # Clean
                                id_regex = ".*",
                                sep_name = " ", # Separate First name and Surname
                                # Adjust Grade
                                new_max_grade = NULL,
                                round_digits = 3, # If NULL = no round
                                # Filter Grades
                                choose_grade = c("max", "min", "mean", "all"),
                                choose_time = c("first", "last", "all"),
                                force_grade = F,
                                sep_col = "_" # Separation for State and Grade column names

) {

  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_rep <- purrr::every(data, is_report)
  if(!is_data_rep) stop("Elements of `data` must be Moodle Quiz report",call. = F)

  has_grade_col <- purrr::every(data, ~is_regex_in_names(.x, "Grade"))
  if(!has_grade_col) stop("Elements of `data` must have 'Grade' column.", call. = F)

  is_nyg_err <- all(!force_grade, purrr::every(data, is_some_grade_nyg))
  if(is_nyg_err) stop("Some students are 'Not yet graded'. If you want to grade anyway, choose `force_grade = TRUE`.", call. = F)

  # If new_max_grade = NULL
  if(is.null(new_max_grade)){
    new_max_grade <- replicate(length(data), NULL)
    tot_max_grade <- purrr::map_dbl(data, get_max_grade) %>% sum()

  }else{
    # If supply new_max_grade as numeric vector
    if(length(new_max_grade) != length(data)) stop("length of `new_max_grade` must equal to length of `data`", call. = F)
    tot_max_grade <- sum(new_max_grade)
  }

  data_ls <- data %>%
    purrr::map2(.y = new_max_grade,
                ~combine_grades.data.frame(.x,
                                           id_regex = id_regex,
                                           sep_name = sep_name,
                                           new_max_grade = .y,
                                           round_digits = round_digits,
                                           choose_grade = choose_grade,
                                           choose_time = choose_time,
                                           force_grade = force_grade
                )
    )

  data_ls %>%
    purrr::map(~dplyr::select(.x, Name, ID, State, tidyselect::starts_with("G"))) %>%
    rename_with_ls_df_names(tidyselect::starts_with("S"), sep = sep_col) %>%
    rename_with_ls_df_names(tidyselect::starts_with("G"), sep = sep_col) %>%
    # Join
    purrr::reduce(dplyr::full_join, by = c("Name", "ID")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate("Total_{tot_max_grade}" := sum(
      dplyr::c_across(tidyselect::vars_select_helpers$where(is.numeric)), na.rm = T)
    ) %>%
    dplyr::ungroup()

}


# Data.frame Method -------------------------------------------------------

#' @export
combine_grades.data.frame <- function(data,
                                      # Clean
                                      id_regex = ".*",
                                      sep_name = " ", # Separate First name and Surname
                                      # Adjust Grade
                                      new_max_grade = NULL,
                                      round_digits = 3, # If NULL = no round
                                      # Filter Grades
                                      choose_grade = c("max", "min", "mean", "all"),
                                      choose_time = c("first", "last", "all"),
                                      force_grade = F,
                                      ... # absorb sep_col arg
) {

  if(!is_report(data)) stop("`data` is not a Moodle Quiz report.", call. = F)

  has_grade_col <- is_regex_in_names(data, "Grade")
  if(!has_grade_col) stop("`data` must has 'Grade' column.", call. = F)

  is_nyg_err <- all(!force_grade, is_some_grade_nyg(data))
  if(is_nyg_err) stop("Some students are 'Not yet graded'. If you want to grade anyway, choose `force_grade = TRUE`.", call. = F)
  # Clean
  data %>%
    clean_moodle(id_regex = id_regex,
                 sep_name = sep_name, force_numeric = TRUE) %>%
    # Adjust Grade
    adj_grades_moodle(new_max_grade = new_max_grade,
                      round_digits = round_digits) %>%
    # Filter Grade
    filter_grades_moodle(choose_grade = choose_grade, choose_time = choose_time)

}


# Helper: Adjuste Grade ---------------------------------------------------


#' Adjust Grade
#'
#' @param data_cleaned A cleaned data.frame
#' @param new_max_grade Length 1 numeric vector indicate new maximum grade, If `NULL` use old maximum grade
#' @param round_digits Length 1 numeric vector indicate digits to round grade, If `NULL` no rounding.
#'
#' @return A data.frame
#'
adj_grades_moodle <- function(data_cleaned,
                              new_max_grade = NULL,
                              round_digits = 3 # If NULL, no round
) {
  ## Check Report type
  is_resp_rep <- stringr::str_detect(names(data_cleaned), "R") %>% any()
  is_grades_rep <- stringr::str_detect(names(data_cleaned), "Q") %>% any()
  ## Symbol for Grade_xx column
  Grade_col <- names(data_cleaned) %>% stringr::str_subset("Grade") %>% rlang::sym()
  ## Old maximum Grade
  old_max_grade <- get_max_grade(data_cleaned)
  ## If not adjust grade -> New = Old
  if(is.null(new_max_grade)){
    new_max_grade <- old_max_grade
  }
  ## Grade adjust factor
  adj_factor <- c(new_max_grade/old_max_grade)
  ## New Grade col name
  Grade_col_new <- rlang::sym(paste0("Grade_", new_max_grade))
  # Adjust Grade (Responses Report)
  if(is_resp_rep){
    data_adj_gr <- data_cleaned %>%
      dplyr::mutate(!!Grade_col_new := !!Grade_col * adj_factor,
                    .keep = "unused", .after = State)
  }
  # Adjust Grade (Grades Report)
  if(is_grades_rep){
    data_adj_gr <- data_cleaned %>%
      # Remove Old Grade_xx Column
      dplyr::select(!tidyselect::starts_with("G")) %>%
      # Adjust New Score for Each Questions
      dplyr::mutate(dplyr::across(tidyselect::starts_with("Q"), ~.x * adj_factor)) %>%
      # Row Sum those Questions to New Grade_xx column
      dplyr::rowwise() %>%
      dplyr::mutate(!!Grade_col_new := sum(
        dplyr::c_across(tidyselect::starts_with("Q")), na.rm = T
      ), .after = State) %>%
      dplyr::ungroup()
  }
  # Round
  if(is.null(round_digits)) return(data_adj_gr)
  data_adj_gr %>%
    dplyr::mutate(dplyr::across(
      c(tidyselect::starts_with("G"), tidyselect::starts_with("Q")),
      ~round(.x, digits = round_digits))
    )

}


# Helper: Filter grades ---------------------------------------------------



#' Filter Grades
#'
#' @param data_cleaned A cleaned data.frame of Moodle Grades report
#' @param choose_grade (Character) To filter grade of each student
#' @param choose_time (Character) To filter attempt of each student
#'
#' @return A data.frame
#'
filter_grades_moodle <- function(data_cleaned,
                                 choose_grade = c("max", "min", "mean", "all"),
                                 choose_time = c("first", "last", "all")
) {

  choose_grade <- rlang::arg_match(choose_grade)
  choose_time <- rlang::arg_match(choose_time)
  # Get Grades Column name
  Grade_col <- stringr::str_subset(names(data_cleaned), "G") %>% rlang::sym()

  # Grouped filter by Score of each student
  filt_expr_1 <- switch (choose_grade,
                         "max" = { rlang::expr(!!Grade_col == max(!!Grade_col)) },
                         "min" = { rlang::expr(!!Grade_col == min(!!Grade_col))},
                         "mean" = { rlang::expr(!!Grade_col == mean(!!Grade_col))},
                         "all" = { rlang::expr(!!Grade_col == !!Grade_col)},
                         stop("`choose_grade` must be one of 'max', 'min', 'mean', 'all'", call. = F)
  )
  # Grouped filter by Started Time of each student
  filt_expr_2 <- switch (choose_time,
                         "first" = { rlang::expr(Started == min(Started)) },
                         "last" = { rlang::expr(Started == max(Started)) },
                         "all" = { rlang::expr(Started == Started) },
                         stop("`choose_time` must be one of 'first', 'last', 'all'", call. = F)
  )

  data_cleaned %>%
    dplyr::group_by(Name, ID) %>%
    dplyr::filter(!!filt_expr_1) %>%
    dplyr::filter(!!filt_expr_2) %>%
    dplyr::ungroup()

}
