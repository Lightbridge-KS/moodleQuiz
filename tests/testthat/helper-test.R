#' Convert column names to tibble
#'
#' @param nms Character vector
#'
#' @return A tibble with `nms` as column names
colnm_to_tbl <- function(nms){

  x <- seq_along(nms)
  names(x) <- nms
  tibble::as_tibble_row(x)

}


#' Expect Regex in Names of Object
#'
#' Test whether a regular expression can be matched in object names
#'
#' @param obj object to test
#' @param regex regular expression
#' @param type type of match:
#' * "any": `regex` must match at least 1 names
#' * "all": `regex` must match all names
#'
#' @return object of `testthat::expect_true()`
#'
expect_regex_in_names <- function(obj,
                                  regex,
                                  type = c("any", "all")
                                  ) {

  type <- match.arg(type)

  nms <- names(obj)
  lgls <- stringr::str_detect(nms, regex)
  lgl <- do.call(type, as.list(lgls))

  testthat::expect_true(lgl)

}
