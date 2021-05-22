#' Is Regular Expressions presented in object names
#'
#' Vectorized testing for regular expressions.
#' Are all of the regular expression can be matched to object names or not?
#'
#' @param x An object to test
#' @param regex Character vector, specify regular expressions
#' @param verbose If `TRUE`, message you that in which components of the object names is/are not match by `regex`
#'
#' @return Logical, if `TRUE` all of the `regex` can be matched to at least one element of names of `x`.
#'
is_regex_in_names <- function(x, regex, verbose = F){

  nm <- names(x)
  lgl_ls <- purrr::map(nm, ~stringr::str_detect(.x, regex))
  lgl_ls_t <- purrr::map(purrr::transpose(lgl_ls), ~unlist(.x, recursive = F))
  lgl_vctr <- purrr::map_lgl(lgl_ls_t, any)

  if(verbose && !all(lgl_vctr)){
    no_match <- regex[which(!lgl_vctr)]
    message("The following not presented in `x`")
    print_msg(no_match, sep = ", ")
  }

  lgl_vctr

}

#' Print Message by a Separator
#'
#' Print message from character vector by a given separator.
#'
#' @param x A character vector contained messages to print
#' @param ... Pass to `...` of `message()`
#' @param sep Separator
#' @param domain Arg of `message()`
#' @param appendLF Arg of `message()`
#'
#' @return Messages
#'
print_msg <- function(x, ... , sep = "\n", domain = NULL, appendLF = TRUE){

  x_comb <- purrr::reduce(x, paste, sep = sep)
  message(x_comb, ..., domain = domain, appendLF = appendLF)

}