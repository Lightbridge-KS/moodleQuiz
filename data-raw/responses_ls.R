## Simulate Responses Data

library(dplyr)
library(purrr)

source("data-raw/simulation_helper.R")


# Fun: Sample Tarzan Data -------------------------------------------------


#' Sample Text from "The Return of Tarzan"
#'
#' using ` gutenbergr::gutenberg_download()` to download text
#'
#' @param n Number of elements in character vector to return
#' @param dash_random How many random dashes in the output
#'
#' @return a character vector
#'
sample_tarzan <- function(n = 26, dash_random = 2) {

  # If object `tarzan` not exist, Download it.
  if(!exists("tarzan")){
    tarzan <<- gutenbergr::gutenberg_download(81)
  }

  tz <- tarzan$text %>%
    dplyr::na_if("") %>%
    na.omit() %>%
    # Not select text starts with "Chapter"
    stringr::str_subset("\\s?Chapter", negate = TRUE) %>%
    sample(n)

  ## Randomly replace  text with "-"
  tz[sample(n, dash_random, replace = FALSE)] <- "-"
  tz

}


# Fun: Simulate Tarzan Response -------------------------------------------


#' Simulate Tarzan Response in a Data Frame
#'
#' @param n number of rows to simulate
#' @param resp_max number of `Response x` columns
#'
#' @return A data frame with 2 columns types:
#' * `Grade/x.xx`
#' * `Response x` from 1 to `resp_max`
#'
simulate_resp_tarzan <- function(n = 26, resp_max = 5){

  df_resp <- seq_len(resp_max) %>%
    setNames(paste0("Response ", seq_len(resp_max))) %>%
    purrr::map_dfc(~ sample_tarzan(n = n, dash_random = 2))

  gr_col_nm <- glue::glue("Grade/{resp_max}.00")
  df_gr <- tibble::tibble(!!gr_col_nm := "Not yet graded")

  dplyr::bind_cols(df_gr, df_resp)

}


# Fun: Simulate Response DF -----------------------------------------------


#' Simulate Response Data Frame
#'
#'
#' @param seed Seed of columns involving time, "Grade/xx.x", and "Response x"
#' @param seed_demo Seed of demographic column (intended to be fixed)
#' @param resp_max number of `Response x` columns
#'
#' @return 26 rows data frame of simulated response report
#'
simulate_resp_report_df <- function(seed = 1,
                                    seed_demo = 1,
                                    resp_max = 5
){

  # State column fixed
  df_state <- tibble::tibble(State = "Finished")

  set.seed(seed_demo)
  # from `Surname` to `Email address`
  df_demo <- simulate_moodle_demo(26)

  set.seed(seed)
  # Time Column
  df_time <- random_time_moodle_df(26, time_taken = c(10,20))
  # Grade/x.xx and Responses x column
  df_gr_resp <- simulate_resp_tarzan(26, resp_max = resp_max)

  dplyr::bind_cols(
    df_demo, df_state, df_time, df_gr_resp
  )


}


# Fun: Simulate Response List of DFs --------------------------------------


#' Simulate Responses List of Data Frame
#'
#' @param n number of DFs to return
#'
#' @return a list of DF
#' Each DF has 26 rows, same students (shuffled), but different Time, Grades, and Response columns.
#'
simulate_resp_report_lsdf <- function(n = 2) {

  ls <- vector("list", n)

  for (i in 1:n) {

    # Simulate Each DF (fixing demographic data)
    df <- simulate_resp_report_df(seed = i, seed_demo = 1, resp_max = i+1)
    # Shuffle Rows
    df_shuffled <- df %>% dplyr::slice_sample(prop = 1)
    # Note: Response report Don't have last row as "Overall average"

    ls[[i]] <- df_shuffled

  }

  names(ls) <- paste0("Quiz_", 1:n)
  ls

}


# Execute -----------------------------------------------------------------

set.seed(123)

responses_ls <- simulate_resp_report_lsdf(n = 2)


usethis::use_data(responses_ls, overwrite = TRUE)
