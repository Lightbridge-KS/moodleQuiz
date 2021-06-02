### Helper Function to Simulate Data



# Demographic -------------------------------------------------------------


#' Simulate Demographic Data for Moodle report
#'
#' @param n number of rows
#' @param from_ymd start date
#' @param to_ymd end date
#' @param time_taken numeric vector length 2: to sample time taken in this range
#' @param units unis of `time_taken`
#' @param sort sort or not
#'
#' @return A data.frame with columns:
#'   "Surname", "First name", "Institution", "Department", "Email address"
simulate_moodle_demo <- function(n = 5,
                                 from_ymd = Sys.Date(),
                                 to_ymd = c(Sys.Date() + 1),
                                 time_taken = c(0, 10),
                                 units = "minute",
                                 sort = F
) {

  df_demo <- tibble::tibble(Name = randomNames::randomNames(n, sample.with.replacement = F),
                            "Institution" = rep(NA, n),
                            "Department" = rep(NA, n),
                            "Email address" = paste0("u", sprintf("%03d", seq_len(n)), "@example.com")
  ) %>%
    tidyr::separate(Name, into = c("Surname", "First name"), sep = "(\\s)*,(\\s)*")

  df_demo
}


# Time --------------------------------------------------------------------



#' Random Time Component from Moodle Data
#'
#' @param n rows to generate
#' @param from_ymd charater specify `Started on` column
#' @param to_ymd character specify `Completed` column
#' @param time_taken length 2 numeric vector, specify range of `Time taken` column
#' @param units units of `time_taken`
#' @param sort lgl sort or not
#'
#' @return Data.frame with 3 columns: "Started on", "Time taken", "Completed"
random_time_moodle_df <- function(n,
                                  from_ymd = Sys.Date(),
                                  to_ymd = c(Sys.Date() + 1),
                                  time_taken = c(0, 10),
                                  units = "minute",
                                  sort = F
){

  started.dt <- rdate_time(n, from_ymd = from_ymd, to_ymd = to_ymd, sort = sort)
  time_taken.dur <- rDuration(n,
                              from = min(time_taken), to = max(time_taken),
                              units = units, sort = sort)
  completed.dt <- started.dt + time_taken.dur

  tibble::tibble("Started on" = started.dt,
                 "Time taken" = time_taken.dur,
                 "Completed" = completed.dt) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::vars_select_helpers$where(lubridate::is.POSIXct),
                    format_datetime_moodle)
    ) %>%
    dplyr::mutate("Time taken" = format_timetaken_moodle(`Time taken`))


}


# helper ------------------------------------------------------------------


#' Random Date-time
#'
#' @param n number of random date-time
#' @param from_ymd start date
#' @param to_ymd end date
#' @param sort sort or not
#'
#' @return POSIXct of random date-time
rdate_time <- function(n = 1,
                       from_ymd = Sys.Date(),
                       to_ymd = c(Sys.Date() + 1),
                       sort = F) {

  from.date <- lubridate::ymd(from_ymd)
  to.date <- lubridate::ymd(to_ymd)

  diff.int <- sample(lubridate::as.duration(c(to.date - from.date)), n, replace = FALSE)
  if(sort) diff.int <- sort(diff.int)

  from.date + lubridate::as.duration(diff.int)

}

#' Format date-time to Moodle Format in character
#'
#' @param x POSIXct vector
#'
#' @return A character vector
format_datetime_moodle <- function(x){

  day.int <- lubridate::day(x)
  month.chr <- lubridate::month(x, label = TRUE, abbr = FALSE)
  year.int <- lubridate::year(x)
  hour.int <- lubridate::hour(x) # Not as AM or PM
  minute.int <- lubridate::minute(x)

  # Construct D M Y
  dmy.chr <- paste(day.int, month.chr, year.int)
  # Construct h m
  hm.chr <- ifelse(lubridate::am(x),
                   ## AM
                   paste0(hour.int, ":", sprintf("%02d",minute.int), " AM"),
                   ## PM
                   paste0(c(hour.int - 12), ":", sprintf("%02d",minute.int), " PM")
  )

  paste(dmy.chr, hm.chr)

}


#' Random Duration
#'
#' @param n number to randomized
#' @param from integer, start time
#' @param to integer, end time
#' @param units character indicate unit of time
#' @param sort sort or not
#'
#' @return Duration class vector
rDuration <- function(n = 1, from = 0, to = 10, units = "minute", sort = FALSE) {

  from.Duration <- lubridate::duration(from, units = units)
  to.Duration <- lubridate::duration(to, units = units)

  diff.Duration <- lubridate::as.duration(
    sample(c(to.Duration - from.Duration), n, replace = TRUE)
  )
  if(sort) diff.Duration <- sort(diff.Duration)

  from.Duration + diff.Duration
}


#' Format Duration object to character
#'
#' @param x Duration object
#'
#' @return character for "Time taken" column in Moodle Report
format_timetaken_moodle <- function(x) {

  dt <- lubridate::as_datetime(x)
  hour.int <- lubridate::hour(dt)
  minute.int <- lubridate::minute(dt)
  second.int <- lubridate::second(dt)

  hour.chr <- dplyr::case_when(hour.int == 0 ~ "",
                               hour.int == 1 ~ paste(hour.int, "hour "),
                               hour.int > 1 ~ paste(hour.int, "hours ")
  )

  minute.chr <- dplyr::case_when(minute.int == 0 ~ "",
                                 minute.int == 1 ~ paste(minute.int, "min "),
                                 minute.int > 1 ~ paste(minute.int, "mins "))

  second.chr <- dplyr::case_when(second.int == 0 ~ "",
                                 second.int == 1 ~ paste(second.int, "sec "),
                                 second.int > 1 ~ paste(second.int, "secs "))

  trimws(paste0(hour.chr, minute.chr, second.chr))

}

