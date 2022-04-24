#' Is an object is a List of Data Frame?
#'
#' This function test an object if it is a list of data.frame
#'
#' @param x An object to test
#' @param quiet If `quiet = TRUE` no message display
#'
#' @return logical
#' @noRd
is_list_data.frame <- function(x, quiet = FALSE) {

  # Check if outer layer is data.frame
  if(is.data.frame(x)){
    if(!quiet) message("`x` is a data.frame, not a list of data.frame")
    return( FALSE )
  }

  # Check if outer layer is list
  is_list <- inherits(x, "list") # this return TRUE if list, otherwise FALSE even if DF
  if(!is_list){
    if(!quiet) message("`x` is not a list")
    return( is_list )
  }

  # Check if every elements of that list inherit data.frame class
  is_element_df <- all(purrr::map_lgl(x, ~inherits(.x, "data.frame")))
  if(!is_element_df){
    if(!quiet) message("`x` is a list but its element is not all data.frame")
    return( is_element_df )
  }

  is_list & is_element_df
}


#' Is an object is a Named List of Data Frame?
#'
#' This function test an object if it is a Named list of data.frame
#'
#' @param x An object to test
#' @param quiet If `quiet = TRUE` no message display
#'
#' @return logical
#' @noRd
is_named_list_data.frame <- function(x, quiet = FALSE) {

  # Check if outer layer is data.frame
  if(is.data.frame(x)){
    if(!quiet) message("`x` is a data.frame, not a list of data.frame")
    return( FALSE )
  }

  # Check if outer layer is list
  is_list <- inherits(x, "list") # this return TRUE if list, otherwise FALSE even if DF
  if(!is_list){
    if(!quiet) message("`x` is not a list")
    return( is_list )
  }
  # Check if list has names
  if(is.null(names(x))){
    if(!quiet) message("`x` is list but has no names")
    return( FALSE )
  }

  # Check if every elements of that list inherit data.frame class
  is_element_df <- all(purrr::map_lgl(x, ~inherits(.x, "data.frame")))
  if(!is_element_df){
    if(!quiet) message("`x` is a named list but its element is not all data.frame")
    return( is_element_df )
  }

  is_list & is_element_df
}
