#' Moodle Grades Report Example
#'
#' Example for [Moodle Grades report](https://docs.moodle.org/311/en/Quiz_reports).
#' Student's scores were retrieved form real world data, but student names, email address and time are randomly
#' simulated.
#'
#'
#' @format A named list of 3 data.frame each has the following columns:
#' * \strong{"Surname"}: surname of students (not real)
#' * \strong{"First name"}: first name of students (not real)
#' * \strong{"Institution"}: `NA`s
#' * \strong{"Department"}: `NA`s
#' * \strong{"Email address"}: Student's email address that has numeric student ID in it.
#' * \strong{"State"}: State of the quiz
#' * \strong{"Started on"}: Quiz start time
#' * \strong{"Time taken"}: Elapsed time
#' * \strong{"Completed"}: Quiz end time
#' * \strong{"Grade/x"}: Student's score with "x" to indicate quiz maximum
#' * \strong{"Q. n /x"}: Score of each questions; "n" indicate question number, "x" indicate maximum of each questions.
"grades_ls"
