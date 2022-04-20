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
