### Count Answers from Response Columns

# Count Cloze Answers ------------------------------------------------------


#' Count Answers from Cloze column
#'
#' @param data A data.frame of Moodle Responses report
#' @param col (quoted or unquoted) A column that contain Cloze answers
#' @param count_cloze_parts (Logical) If `TRUE`: count answers in each parts of Cloze column.
#' @param sort (Logical) sorted or not
#'
#' @return A data.frame
#'
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
