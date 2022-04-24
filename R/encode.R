#' Encode State column of Moodle Quiz report
#'
#' @param df_cleaned A cleaned data.frame of Moodle Quiz report
#' @param state A character vector to match values in "State" column of Moodle Quiz report
#' @param encode An encoding numeric vector corresponding to `state`.
#'   For example: by default, in the "State" column, "Finished" values will be encoded as `1`, and
#'   "In progress" will be encoded as `0`.
#' @param choose_encode A character to filter student's attempt by the `encode`ing.
#'   * \strong{"max"} (default): return rows that have maximum encoding of each students.
#'   * \strong{"min"}: return rows that have mininum encoding of each students.
#'   * \strong{"all"}: no filter applied, return all rows.
#'
#' @param choose_time A character to filter student's attempt by started time (determined by "Started on" column in Moodle Quiz report).
#'   This filter applies **after** `choose_encode` has been applied to the data.
#'   * \strong{"first"} (default): return rows that represent first attempt of each students.
#'   * \strong{"last"}: return rows that represent last attempt of each students.
#'   * \strong{"all"}: no filter applied, return all rows after applying `choose_encode`
#'
#' @return A data.frame that has been encoded and filtered
#' @noRd
encode_moodle <- function(df_cleaned,
                          state = c("Finished", "In progress"),
                          encode = c(1,0),
                          choose_encode = c("max", "min", "all"),
                          choose_time = c("first", "last", "all")
) {

  if(length(state) != length(encode)) stop("`state` and `encode` must have same length", call. = F)

  choose_encode <- rlang::arg_match(choose_encode)
  choose_time <- rlang::arg_match(choose_time)

  df_encoded <- df_cleaned %>%
    dplyr::mutate(Encode = encoder(State, state, encode), .after = "State")

  # "max", "min", "all" = maximum, minimum or all of encoding
  filt_expr_1 <- switch (choose_encode,
                         "max" = {rlang::expr(Encode == max(Encode))},
                         "min" = {rlang::expr(Encode == min(Encode))},
                         "all" = {rlang::expr(Encode == Encode)},
                         stop("Must be one of `max`, `min`, `all`",
                              call. = F)
  )
  # "first", "last" = first or last attempt
  filt_expr_2 <- switch (choose_time,
                         "first" = {rlang::expr(Started == min(Started))},
                         "last" = {rlang::expr(Started == max(Started))},
                         "all" = {rlang::expr(Started == Started)},
                         stop("Must be one of `max`, `min`, `all`",
                              call. = F)
  )

  df_encoded %>%
    dplyr::group_by(Name, ID) %>%
    dplyr::filter(!!filt_expr_1) %>%
    dplyr::filter(!!filt_expr_2) %>%
    dplyr::ungroup()

}



# Helper: Encoder ---------------------------------------------------------


#' Encoding function
#'
#' @param x vector: input data to be matched.
#' @param match vector: matching value to `x`
#' @param encode vector: encoding vector same length as `match`
#'
#' @return encoded vector
#' @details Elements that not match will return `NA`.
#' @noRd
encoder <- function(x, # Any vector
                    match,
                    encode = match # Encode that pair with match
) {

  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)

  df <- data.frame(match = match, encode = encode)
  index <- match(x, df$match)

  df$encode[index]

}
