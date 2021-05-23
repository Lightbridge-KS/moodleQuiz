# Generic: combine responses ----------------------------------------------



#' Combine Student's Responses
#'
#' [combine_resp()] is a generic function that combine student responses from [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report).
#' If the Moodle responses report has response column(s) that contained [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type), you have an option to split them into different parts (one column for each).
#' Other functionality are similar to [check_sub()] such as: encode & filter student's attempts by state (i.e., "Finished" or "In progress") and started time ("Started on" column),
#' cleans column names for easy manipulation, extracts student ID from "Email address", and unites "First name" and "Surname" column into "Name".
#'
#' @param data A data.frame **or** named list of data.frame of [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report)
#' @param id_regex (Character) A regular expression used to extract ID from column "Email address" in the Moodle Quiz report. The default is "`.*`" meaning all characters.
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
#' @param split_cloze (Logical) If the `data` has at least one response column(s) that contained [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type), `split_cloze = TRUE` will split them into different parts (one column for each).
#'  If the `data` has no Cloze response columns, `split_cloze = TRUE` will result in error message. If `FALSE` (default), no Cloze column will be split.
#' @param part_glue (Character) If `split_cloze = TRUE` and cloze response columns is presented, `part_glue` represent a character that *glues* the names of "Response" column and each "Parts" of Cloze columns together.
#' @param sep_col (Character) If `data` is a named list of data.frame, `sep_col` indicate a character separation between names of list and "State" or "Response" columns.
#'
#' @return **A data.frame**, its output content is determined by class of its first argument: `data`.
#'   * If the `data` is a data.frame; the output is an encoded, filtered, and cleaned data.frame of Moodle Responses report.
#'   * If the `data` is a named list of data.frame; the output is the same as previously described, but all Moodle Quiz reports are [full-joined](https://dplyr.tidyverse.org/reference/mutate-joins.html) together by column "Name" and "ID".
#'   So that, "Response" columns from each data.frame are sit together in a single data.frame. The resulting response columns are in this format (default): "<listNames>-Responses-<number>"
#' @export
#'
#' @examples NULL
combine_resp <- function(data,
                         # Clean
                         id_regex = ".*", # Extract ID from Email
                         sep_name = " ", # Separate First name and Surname
                         # Encode
                         state = c("Finished", "In progress"),
                         encode = c(1,0),
                         choose_encode = c("max", "min", "all"),
                         choose_time = c("first", "last", "all"),
                         # Split cloze
                         split_cloze = F,
                         part_glue = "_part_",
                         # Extra for list method
                         sep_col = "_" # Separation for State and Response column names
) {

  UseMethod("combine_resp")
}


# List method -------------------------------------------------------------


#' @export
combine_resp.list <- function(data,
                              # Clean
                              id_regex = ".*", # Extract ID from Email
                              sep_name = " ", # Separate First name and Surname
                              # Encode
                              state = c("Finished", "In progress"),
                              encode = c(1,0),
                              choose_encode = c("max", "min", "all"),
                              choose_time = c("first", "last", "all"),
                              # Split cloze
                              split_cloze = F,
                              part_glue = "_part_",
                              # Extra
                              sep_col = "_" # Separation for State and Response column names
) {

  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_resp_rep <- purrr::every(data, is_responses_report)
  if(!is_data_resp_rep) stop("Elements of `data` must be Moodle Responses report",call. = F)
  data %>%
    purrr::map(~combine_resp.data.frame(.x,
                                        # Clean
                                        id_regex = id_regex, # Extract ID from Email
                                        sep_name = sep_name, # Separate First name and Surname
                                        # Encode
                                        state = state,
                                        encode = encode,
                                        choose_encode = choose_encode,
                                        choose_time = choose_time,
                                        # Split cloze
                                        split_cloze = split_cloze,
                                        part_glue = part_glue
    )) %>%
    purrr::map(~dplyr::select(.x, Name, ID, State, tidyselect::starts_with("R"))) %>%
    # Prefix column "Responses" and "State" with names(list)
    rename_with_ls_df_names(.cols = tidyselect::starts_with("R"), sep = sep_col) %>%
    rename_with_ls_df_names(.cols = tidyselect::starts_with("S"), sep = sep_col) %>%

    purrr::reduce(dplyr::full_join, by = c("ID", "Name"))

}




# Data.frame method -------------------------------------------------------


#' @export
combine_resp.data.frame <- function(data,
                                    # Clean
                                    id_regex = ".*", # Extract ID from Email
                                    sep_name = " ", # Separate First name and Surname
                                    # Encode
                                    state = c("Finished", "In progress"),
                                    encode = c(1,0),
                                    choose_encode = c("max", "min", "all"),
                                    choose_time = c("first", "last", "all"),
                                    # Split cloze
                                    split_cloze = F,
                                    part_glue = "_part_",
                                    ... # to absorb sep_col argument
) {

  if(!is_responses_report(data)) stop("`data` is not a Moodle Responses report.", call. = F)

  data_enc <- data %>%
    clean_moodle(id_regex = id_regex, sep_name = sep_name,
                 force_numeric = FALSE, dash_na = FALSE) %>%
    encode_moodle(state = state, encode = encode,
                  choose_encode = choose_encode,
                  choose_time = choose_time)

  # If not split cloze column; return
  if(!split_cloze) return(data_enc)
  # If choose to split_cloze and not found cloze col; Error
  if(!has_cloze_col(data_enc)) stop("This Moodle Responses report has no Cloze column to split", call. = F)

  split_cloze(data_enc, part_glue = part_glue)

}
