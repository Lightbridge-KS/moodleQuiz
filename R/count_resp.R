
# Generic: Count Responses ------------------------------------------------


#' Count Student's Responses
#'
#'
#' [count_resp()] is a generic function that count responses per student from [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report).
#' Similar to [combine_resp()], if the Moodle responses report has response column(s) that contained [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type),
#' you have an option to count them as a whole (1 Cloze column = 1 count) or as individual parts (count each parts of Cloze column, individually).
#' Other functionality are similar to [check_sub()] such as: encode & filter student's attempts by state (i.e., "Finished" or "In progress") and started time ("Started on" column),
#' cleans column names for easy manipulation, extracts student ID from "Email address", and unites "First name" and "Surname" column into "Name".
#'
#' @param data A data.frame **or** named list of data.frame of [Moodle Responses report(s)](https://docs.moodle.org/311/en/Quiz_reports) (not Grades report)
#' @param extract_id_from (Character) Choose 1 column to extract ID from
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
#' @param count_cloze_parts (Logical) If a Moodle Responses report has at least one response column(s) that contained [embedded answers (Cloze)](https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type),
#'   set `count_cloze_parts = TRUE` will count individual parts of each Cloze columns. If `FALSE`, then it will count each Cloze column as 1 count.
#' @param filter_count (Character) One of "max" or "all". This filter option will apply after filter by `choose_encode` and `choose_time`.
#'   * \strong{"max"}: filter rows that has maximum response count per student. If there are ≥ 1 rows, filter the first one based on `Started`.
#'   * \strong{"all"}: No further filter apply.
#' @param sep_col (Character) If `data` is a named list of data.frame, `sep_col` indicate a character separation between names of list and "State" or "Count_resp" columns.
#'
#'
#' @return **A data.frame**, its output content is determined by class of its first argument: `data`.
#'   * If the `data` is a data.frame; the output is an encoded, filtered, and cleaned data.frame of Moodle Responses report.
#'   And the "Count_resp" column is added with maximum number of count appended at the column name.
#'   * If the `data` is a named list of data.frame; the output is the same as previously described, but all Moodle Quiz reports are [full-joined](https://dplyr.tidyverse.org/reference/mutate-joins.html) together by column "Name" and "ID".
#'   So that, "Count_resp" columns from each data.frame are sit together in a single data.frame, and "Total" column is added at the last column to indicate total count.
#'
#' @details **Counting Mechanism**:
#'   [count_resp()] gives 1 count to 1 responses cell if one or more characters were found.
#'   Blank or "-" (dash) response will not be counted.
#'
#' @export
#'
#' @examples
#' # Count Responses of Quiz 1
#' count_resp(responses_ls$Quiz_1,
#'            id_regex = "[:digit:]+")
#'
#' # Count Responses from All Quizzes
#' count_resp(responses_ls,
#'            id_regex = "[:digit:]+")
count_resp <- function(data,
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
                       # TRUE = count individual parts of cloze responses
                       count_cloze_parts = F,
                       filter_count = c("max", "all"),
                       sep_col = "_" # To List method: `sep_col`
) {

  UseMethod("count_resp")

}


# List method -------------------------------------------------------------

#' @export
count_resp.list <- function(data,
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
                            # TRUE = count individual parts of cloze responses
                            count_cloze_parts = F,
                            filter_count = c("max", "all"),
                            sep_col = "_"
) {
  # Validate Responses report
  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_resp_rep <- purrr::every(data, is_responses_report)
  if(!is_data_resp_rep) stop("Elements of `data` must be Moodle Responses report",call. = F)
  # If want to count cloze parts, all element of `data` must have cloze
  is_some_no_cloze <- purrr::some(data, ~!has_cloze_col(.x))
  if(count_cloze_parts && is_some_no_cloze) stop("Some elements of Moodle Responses report has no Cloze column to count", call. = F)

  # Total Responses Count
  tot_resp_count <- data %>%
    purrr::map_int(~get_max_resp(.x, count_cloze_parts = count_cloze_parts)) %>%
    sum()

  data %>%
    purrr::map(
      ~count_resp.data.frame(.x,
                             id_regex = id_regex,
                             extract_id_from = extract_id_from,
                             sep_name = sep_name,
                             state = state, encode = encode,
                             choose_encode = choose_encode,
                             choose_time = choose_time,
                             count_cloze_parts = count_cloze_parts,
                             filter_count = filter_count)
    ) %>%
    purrr::map(~dplyr::select(.x, Name, ID, State, tidyselect::starts_with("C"))) %>%
    # Prefix "State" and "Count" column name
    rename_with_ls_df_names(tidyselect::starts_with("S"), sep = sep_col) %>%
    rename_with_ls_df_names(tidyselect::starts_with("C"), sep = sep_col) %>%
    # Join
    purrr::reduce(dplyr::full_join, by = c("ID", "Name")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "Total_{tot_resp_count}":= sum(dplyr::c_across(
        tidyselect::vars_select_helpers$where(is.numeric)), na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()


}


# Data.frame method -------------------------------------------------------

#' @export
count_resp.data.frame <- function(data,
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
                                  # TRUE = count individual parts of cloze responses
                                  count_cloze_parts = F,
                                  filter_count = c("max", "all"),
                                  ...
) {

  filter_count <- match.arg(filter_count)

  if(!is_responses_report(data)) stop("`data` is not a Moodle Responses report.", call. = F)

  if(count_cloze_parts && !has_cloze_col(data)) stop("This Moodle Responses report has no Cloze column to count", call. = F)

  tot_len <- get_max_resp(data, count_cloze_parts = count_cloze_parts)
  ### Count Resp Colum Name
  count_resp_colnm <- rlang::sym(paste0("Count_Resp_", tot_len))

  data_comb <- data %>%
    ### Passed to Combine Responses DF
    combine_resp.data.frame(split_cloze = count_cloze_parts,
                            extract_id_from = extract_id_from,
                            id_regex = id_regex, sep_name = sep_name,
                            state = state, encode = encode,
                            choose_encode = choose_encode, choose_time = choose_time
    ) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("R"), ~as.character(dplyr::na_if(.x, "-")))
    )

  data_counted <- data_comb %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!count_resp_colnm := sum(!is.na(dplyr::c_across(tidyselect::starts_with("R"))))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!tidyselect::starts_with("R"))

  ## NOT filter max count
  if(filter_count == "all") return(data_counted)
  ## Filter Maximum Count
  if(filter_count == "max"){
    data_counted %>%
      dplyr::group_by(Name, ID) %>%
      # Choose Max Count per Student Name, ID
      dplyr::filter(!!count_resp_colnm == max(!!count_resp_colnm)) %>%
      ## If more than 1 max count, choose the first one
      dplyr::filter(Started == min(Started)) %>%
      ## If started at the same time, choose the first row
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  }

}


