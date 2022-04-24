#' Detect and Split all Cloze columns in a Data Frame
#'
#' This is a higher level function than [moodleQuiz::split_cloze_col()], because it automatically finds all
#' embedded answers (Cloze) response columns and split all of them into multiple parts (one column for each part).
#'
#' @param data A data.frame of Moodle Responses report that has at least one embedded answer (Cloze) responses column.
#' @param part_glue (Character) This indicate characters that separate different "parts" of a Cloze column.
#'
#' @return A data.frame with cloze column splitted into multiple parts
#' @noRd
#'
split_cloze <- function(data, part_glue = "_part_") {

  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  if(!has_cloze_col(data)) stop("`data` must have at leat one Cloze Response column.",
                                call. = F)
  cloze_col_nm <- get_cloze_col_names(data)
  # Split data into response col and non-responses col
  resp_df <- data %>% dplyr::select(tidyselect::starts_with("R"))
  non_resp_df <- data %>% dplyr::select(!tidyselect::starts_with("R"))

  # Get Cloze col names and col number; relative to splited df
  cloze_resp_no <- as.integer(stringr::str_extract(cloze_col_nm, "[:digit:]+"))
  # Interger vector response No
  resp_no <- as.integer(stringr::str_extract(names(resp_df), "[:digit:]+"))
  cloze_i <- match(cloze_resp_no, resp_no) # Index of cloze resp
  # Get Non-Cloze col names and col number; relative to splited df
  non_cloze_col_nm <- names(resp_df)[-cloze_i]
  non_cloze_resp_no <- resp_no[-cloze_i]  # Resp No of non-cloze

  # Ls_df Splited cloze responses (one column for each ls_df)
  cloze_ls_df <- cloze_col_nm %>%
    purrr::map(
      ~split_cloze_col(dplyr::select(resp_df, !!.x), !!.x ,
                       split_type = "cols", part_glue = part_glue)
    ) %>%
    stats::setNames(cloze_resp_no)

  # Ls_df of non-cloze responses (one column for each ls_df)
  non_cloze_ls_df <- non_cloze_col_nm %>%
    purrr::map(~dplyr::select(resp_df, !!.x)) %>%
    stats::setNames(non_cloze_resp_no)
  # Join list of cloze and non-cloze
  resp_ls_df <- append(cloze_ls_df, non_cloze_ls_df)
  # Order by Name
  resp_ls_df <- resp_ls_df[order(names(resp_ls_df))]

  # Bind everything
  dplyr::bind_cols(non_resp_df, resp_ls_df)
}



# Split single cloze column -----------------------------------------------



#' Split Single Cloze Column
#'
#' An embedded answer (Cloze) response column usually has multiple parts separate by colon and semicolon
#' such as: "part 1: answer 1; part 2: answer 2;". This function split a single Cloze response column into multiple parts.
#'
#' @param data A data.frame of Moodle Responses report that has at least one embedded answer (Cloze) responses column.
#' @param col Quoted or unquoted expression that indicate the Cloze response column name.
#' @param split_type (Character) Indicate output type: "cols" to split into multiple columns, "rows" to split into multiple rows.
#' @param part_glue (Character) If `split_type = "cols"`, this indicate characters that separate different "parts" of a Cloze column.
#'
#' @return A data.frame with cloze column splitted into multiple parts
#' @importFrom rlang :=
#' @noRd
#'
split_cloze_col <- function(data, col,
                            split_type = c("cols","rows"),
                            part_glue = "_part_"
) {

  split_type <- rlang::arg_match(split_type)
  col <- rlang::ensym(col)
  col_labs <- rlang::as_label(col)
  regex <- "part [:digit:]+:"

  # Extract How many "part"; max() to exclude "-" answers
  parts_len <- data[[col_labs]] %>% stringr::str_count(regex) %>% unique() %>% max(na.rm = T)

  data_split_cols <- data %>%
    # If col has dashed -> NA
    dplyr::mutate(!!col := dplyr::na_if(!!col, "-")) %>%
    # Remove all "part"
    dplyr::mutate(!!col := stringr::str_remove_all(!!col, regex)) %>%
    # Separate to cols
    tidyr::separate(!!col, into = paste0(col_labs, part_glue, seq_len(parts_len)), sep = "; ")

  out <- switch (split_type,
                 "cols" = { data_split_cols },
                 "rows" = {
                   col_nm <- paste0(col_labs, "_parts")
                   data_split_cols %>%
                     tidyr::pivot_longer(cols = tidyselect::starts_with(paste0(col_labs, part_glue)),
                                         names_to = col_nm,
                                         names_prefix = paste0(col_labs, part_glue),
                                         values_to = paste0(col_labs,"_answers"))
                 }
  )

  out

}
