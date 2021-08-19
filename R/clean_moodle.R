#' Clean Moodle Quiz report
#'
#' Clean Moodle Quiz report for easier manipulation in `R`. It extracts student ID from "Email address" column;
#' unites "First name" and "Surname" column; converts dash ("-"), which can be presented when student not given answer, to `NA`;
#' and forces "Grade" and "Q. x /x" to numeric value (if any of "Not yet graded", "Requires grading", or "-" is presented it will be `NA`).
#'
#'
#' @param data A data.frame of [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports).
#' @param extract_id (Logical) `TRUE`: An "ID" column will be created by extracting characters from "Email address" column
#'   using regular expression as `id_regex`. If `FALSE`: "Email address" column will be renamed to "Email".
#' @param id_regex (Character) Regular expression used to extract ID from "Email address". The default is ".*" meaning all characters.
#'   If your student email addresses has numeric IDs in them, try "`[:digit:]+`" to extract digits from the email.
#'   **Note**: Regular expression syntax is the same as [stringr](https://github.com/rstudio/cheatsheets/blob/master/strings.pdf).
#' @param sep_name (Character) A character in the new "Name" column that separate original "First name" and "Surname".
#' @param dash_na (Logical) `TRUE`: format dash "-" in any columns to `NA`. If `FALSE`: leave dash "-" as is
#' @param force_numeric (Logical) `TRUE`: force "Grade" and "Q. x /x" columns to numeric type (if any of "Not yet graded", "Requires grading", or "-" is presented it will be `NA`).
#'   If `FALSE`: leave "Grade" and "Q. x /x" columns as is.
#'
#' @return A data.frame with cleaner column names and better formatted data.
#' Although the column names are cleaned, some metadata is lost such as maximum score of quiz and questions.
#' Check out [get_quiz_attr()] for get these metadata.
#' @export
#'
#' @examples
clean_moodle <- function(data,
                         extract_id = TRUE, id_regex = ".*", # Extract ID from Email
                         sep_name = " ", # Separate First name and Surname
                         dash_na = TRUE, # Format Dash "-" to NA
                         force_numeric = TRUE # Force format Grade and Q column to numeric
) {

  if(!is_report(data)) stop("This is not a moodle quiz report.", call. = F)

  q_max_no_df <- get_questions_no_max(data)
  resp_no <- get_responses_no(data)

  df_cleaned_1 <- data %>%
    # Filter out "Overall average" in the last row of Grade report
    dplyr::filter(!is.na(`Email address`)) %>%
    # Select Column that use in all type of Moodler Function
    dplyr::select(tidyselect::all_of(c("First name", "Surname",
                                       "Email address", "State", "Started on")),
                  # Select Grade column (if any)
                  tidyselect::starts_with("G"),
                  # Select Response column (if any)
                  tidyselect::starts_with("Response"),
                  # Select Q. column (if any)
                  tidyselect::matches("Q\\.")) %>%
    tidyr::unite("First name", "Surname", col = "Name", sep = sep_name) %>%
    dplyr::rename(Email = "Email address", Started ="Started on")

  # Replace dash "-" with NA
  if(dash_na){
    df_cleaned_1 <- df_cleaned_1 %>% purrr::map_df(~dplyr::na_if(.x, "-"))
  }

  df_cleaned_1 <- df_cleaned_1 %>%
    # Reformat Stated Date to POSIXct
    dplyr::mutate(Started = lubridate::dmy_hm(Started)) %>%
    # Replace "/" at Grade/xx column
    dplyr::rename_with(.fn = ~stringr::str_replace(.x, "/", "_"))

  # Format Grade column to numeric; Even if it's "Not yet graded" or dashed
  if(force_numeric){
    df_cleaned_1 <- df_cleaned_1 %>%
      dplyr::mutate(
        dplyr::across(tidyselect::starts_with("G"),
                      ~dplyr::na_if(.x, "Not yet graded")),
        dplyr::across(tidyselect::starts_with("G"),
                      ~dplyr::na_if(.x, "-")),
        dplyr::across(tidyselect::starts_with("G"), as.numeric)
      )
  }

  # If Grades Report, rename Q column and remove max
  if(is_grades_report(data)){

    df_cleaned_2 <- df_cleaned_1 %>%
      dplyr::rename_with(.fn = ~paste0("Q", q_max_no_df$q_no),
                         .cols =  tidyselect::starts_with("Q"))
    if(force_numeric){
      # Format Q_xx column to numeric; Even if it's "Requires grading" or dashed
      df_cleaned_2 <- df_cleaned_2 %>%
        dplyr::mutate(
          dplyr::across(tidyselect::starts_with("Q"),
                        ~dplyr::na_if(.x, "Requires grading")),
          dplyr::across(tidyselect::starts_with("Q"),
                        ~dplyr::na_if(.x, "-")),
          dplyr::across(tidyselect::starts_with("Q"), as.numeric)
        )
    }
  }
  # If Responses Report, rename R column
  if(is_responses_report(data)){

    df_cleaned_2 <- df_cleaned_1 %>%
      dplyr::rename_with(.fn = ~paste0("Response_", resp_no),
                         .cols =  tidyselect::starts_with("R"))
  }

  if(!extract_id) return(df_cleaned_2)
  # Extract Numeric ID from Email
  df_cleaned_2 %>%
    dplyr::mutate(ID = as.character(stringr::str_extract(Email, id_regex)),
                  .keep = "unused", .after = Name)

}
