
# Generic: Check Moodle Submission -----------------------------------------------


#' Check Student Submission
#'
#' [check_sub()] is a generic function that check student submission(s) from [Moodle Quiz report(s)](https://docs.moodle.org/311/en/Quiz_reports)
#' (i.e. Grades or Responses report). This function can encode & filter student's attempts by state (i.e., "Finished" or "In progress") and started time ("Started on" column).
#' This function also cleans column names of Moodle Quiz report for easier manipulation, extracts student ID from "Email address" column using regular expression,
#' and unites "First name" and "Surname" column into "Name".
#'
#' @param data A data.frame **or** named list of data.frame of [Moodle Quiz report(s)](https://docs.moodle.org/311/en/Quiz_reports) (i.e. either Grades or Responses report).
#' @param extract_id_from (Character) Choose 1 column to extract ID from
#' @param id_regex (Character) A regular expression used to extract ID from column (choose one) "Email address", "Institution", "Department", or "ID number" in the Moodle Quiz report. The default is "`.*`" meaning all characters.
#'   If your student email addresses has numeric IDs in them, try "`[:digit:]+`" to extract digits from the email.
#'   **Note**: Regular expression syntax is the same as [stringr](https://github.com/rstudio/cheatsheets/blob/master/strings.pdf).
#' @param sep_name A character in the new "Name" column that separate original "First name" and "Surname".
#' @param state A character vector to match values in "State" column of the Moodle Quiz report
#' @param encode An encoding numeric vector corresponding to `state`.
#'   For example: by default, in the "State" column, "Finished" values will be encoded as `1`, and
#'   "In progress" will be encoded as `0`.
#' @param choose_encode A character to filter student's attempt by the `encode`ing.
#'   * \strong{"max"} (default): return rows that have maximum encoding of each students.
#'   * \strong{"min"}: return rows that have minimum encoding of each students.
#'   * \strong{"all"}: no filter applied, return all rows.
#' @param choose_time A character to filter student's attempt by started time (determined by "Started on" column in Moodle Quiz report).
#'   This filter applies **after** `choose_encode` has been applied to the data.
#'   * \strong{"first"} (default): return rows that represent first attempt of each students.
#'   * \strong{"last"}: return rows that represent last attempt of each students.
#'   * \strong{"all"}: no filter applied, return all rows after applying `choose_encode`
#' @param ... argument `sep_col` of `check_sub.list()`, which indicates a character separation between names of list and "state" or "encode" columns.
#' @return **A data.frame**, its output content is determined by class of its first argument: `data`.
#'   * If the `data` is a data.frame; the output is an encoded, filtered, and cleaned data.frame of Moodle Quiz report.
#'   * If the `data` is a named list of data.frame; the output is the same as previously described, but all Moodle Quiz reports are [full-joined](https://dplyr.tidyverse.org/reference/mutate-joins.html) together by column "Name" and "ID".
#'   And, a new column "Total" is computed by the sum of all "...State" columns.
#'
#' @examples NULL
#' @export
check_sub <- function(data,
                      extract_id_from = c("Email address",
                                          "Institution", "Department", "ID number"),
                      id_regex = ".*", # Extract ID from Email
                      sep_name = " ", # Separate First name and Surname
                      state = c("Finished", "In progress"),
                      # value in "State" column to encode
                      encode = c(1,0),
                      # encode argument `state` to
                      choose_encode = c("max", "min", "all"),
                      choose_time = c("first", "last", "all"),
                      ... # passed to `sep_col`
) {

  UseMethod("check_sub")

}

# List Method -------------------------------------------------------------


#' @export
check_sub.list <- function(data,
                           extract_id_from = c("Email address",
                                               "Institution", "Department", "ID number"),
                           id_regex = ".*", # Extract ID from Email
                           sep_name = " ", # Separate First name and Surname
                           # Encode
                           state = c("Finished", "In progress"),
                           encode = c(1,0),
                           choose_encode = c("max", "min", "all"),
                           choose_time = c("first", "last", "all"),
                           sep_col = "_", # Separation of State and Encode column names
                           ...
) {

  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_moodle_rep <- purrr::every(data, is_report)
  if(!is_data_moodle_rep) stop("Elements of `data` must be Moodle quiz report",call. = F)

  nm <- names(data)

  data_ls <- data %>%
    purrr::map(~check_sub.data.frame(.x,
                                     extract_id_from = extract_id_from,
                                     id_regex = id_regex,
                                     sep_name = sep_name,
                                     state = state,
                                     encode = encode,
                                     choose_encode = choose_encode,
                                     choose_time = choose_time))

  data_df <-  data_ls %>%
    purrr::map(~dplyr::select(.x, Name, ID, State, Encode)) %>%
    # Prefix State and Encode column with names(list)
    rename_with_ls_df_names(State, sep = sep_col) %>%
    rename_with_ls_df_names(Encode, sep = sep_col) %>%
    # Join list
    purrr::reduce(dplyr::full_join, by = c("ID", "Name"))

  # Compute Total
  data_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(dplyr::c_across(
      tidyselect::vars_select_helpers$where(is.numeric)), na.rm = T)
    ) %>%
    dplyr::ungroup()
}



# data.frame Method -------------------------------------------------------


#' @export
check_sub.data.frame <- function(data,
                                 # Clean
                                 extract_id_from = c("Email address",
                                                     "Institution", "Department", "ID number"),
                                 id_regex = ".*", # Extract ID from Email
                                 sep_name = " ", # Separate First name and Surname
                                 # Encode
                                 state = c("Finished", "In progress"),
                                 encode = c(1,0),
                                 choose_encode = c("max", "min", "all"),
                                 choose_time = c("first", "last", "all"),
                                 ...
) {

  if(!is_report(data)) stop("`data` is not a moodle quiz report", call. = F)

  data %>%
    clean_moodle(extract_id = TRUE,
                 extract_id_from = extract_id_from,
                 id_regex = id_regex, sep_name = sep_name,
                 dash_na = FALSE, force_numeric = FALSE) %>%
    encode_moodle(state = state,
                  encode = encode,
                  choose_encode = choose_encode,
                  choose_time = choose_time)

}

