% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_resp.R
\name{count_resp}
\alias{count_resp}
\title{Count Student's Responses}
\usage{
count_resp(
  data,
  extract_id_from = c("Email address", "Institution", "Department", "ID number"),
  id_regex = ".*",
  sep_name = " ",
  state = c("Finished", "In progress"),
  encode = c(1, 0),
  choose_encode = c("max", "min", "all"),
  choose_time = c("first", "last", "all"),
  count_cloze_parts = F,
  filter_count = c("max", "all"),
  sep_col = "_"
)
}
\arguments{
\item{data}{A data.frame \strong{or} named list of data.frame of \href{https://docs.moodle.org/311/en/Quiz_reports}{Moodle Responses report(s)} (not Grades report)}

\item{extract_id_from}{(Character) Choose 1 column to extract ID from}

\item{id_regex}{(Character) A regular expression used to extract ID from column "Email address" in the Moodle Quiz report. The default is "\verb{.*}" meaning all characters.
If your student email addresses has numeric IDs in them, try "\verb{[:digit:]+}" to extract digits from the email.
\strong{Note}: Regular expression syntax is the same as \href{https://github.com/rstudio/cheatsheets/blob/master/strings.pdf}{stringr}.}

\item{sep_name}{A character in the new "Name" column that separate original "First name" and "Surname".}

\item{state}{A character vector to match values in "State" column of the Moodle Quiz report}

\item{encode}{An encoding numeric vector corresponding to \code{state}.
For example: by default, in the "State" column, "Finished" values will be encoded as \code{1}, and
"In progress" will be encoded as \code{0}.}

\item{choose_encode}{A character to filter student's attempt by the \code{encode}ing.
\itemize{
\item \strong{"max"} (default): return rows that have maximum encoding of each students.
\item \strong{"min"}: return rows that have minimum encoding of each students.
\item \strong{"all"}: no filter applied, return all rows.
}}

\item{choose_time}{A character to filter student's attempt by started time (determined by "Started on" column in Moodle Quiz report).
This filter applies \strong{after} \code{choose_encode} has been applied to the data.
\itemize{
\item \strong{"first"} (default): return rows that represent first attempt of each students.
\item \strong{"last"}: return rows that represent last attempt of each students.
\item \strong{"all"}: no filter applied, return all rows after applying \code{choose_encode}
}}

\item{count_cloze_parts}{(Logical) If a Moodle Responses report has at least one response column(s) that contained \href{https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type}{embedded answers (Cloze)},
set \code{count_cloze_parts = TRUE} will count individual parts of each Cloze columns. If \code{FALSE}, then it will count each Cloze column as 1 count.}

\item{filter_count}{(Character) One of "max" or "all". This filter option will apply after filter by \code{choose_encode} and \code{choose_time}.
\itemize{
\item \strong{"max"}: filter rows that has maximum response count per student. If there are ≥ 1 rows, filter the first one based on \code{Started}.
\item \strong{"all"}: No further filter apply.
}}

\item{sep_col}{(Character) If \code{data} is a named list of data.frame, \code{sep_col} indicate a character separation between names of list and "State" or "Count_resp" columns.}
}
\value{
\strong{A data.frame}, its output content is determined by class of its first argument: \code{data}.
\itemize{
\item If the \code{data} is a data.frame; the output is an encoded, filtered, and cleaned data.frame of Moodle Responses report.
And the "Count_resp" column is added with maximum number of count appended at the column name.
\item If the \code{data} is a named list of data.frame; the output is the same as previously described, but all Moodle Quiz reports are \href{https://dplyr.tidyverse.org/reference/mutate-joins.html}{full-joined} together by column "Name" and "ID".
So that, "Count_resp" columns from each data.frame are sit together in a single data.frame, and "Total" column is added at the last column to indicate total count.
}
}
\description{
\code{\link[=count_resp]{count_resp()}} is a generic function that count responses per student from \href{https://docs.moodle.org/311/en/Quiz_reports}{Moodle Responses report(s)} (not Grades report).
Similar to \code{\link[=combine_resp]{combine_resp()}}, if the Moodle responses report has response column(s) that contained \href{https://docs.moodle.org/311/en/Embedded_Answers_(Cloze)_question_type}{embedded answers (Cloze)},
you have an option to count them as a whole (1 Cloze column = 1 count) or as individual parts (count each parts of Cloze column, individually).
Other functionality are similar to \code{\link[=check_sub]{check_sub()}} such as: encode & filter student's attempts by state (i.e., "Finished" or "In progress") and started time ("Started on" column),
cleans column names for easy manipulation, extracts student ID from "Email address", and unites "First name" and "Surname" column into "Name".
}
\details{
\strong{Counting Mechanism}:
\code{\link[=count_resp]{count_resp()}} gives 1 count to 1 responses cell if one or more characters were found.
Blank or "-" (dash) response will not be counted.
}
\examples{
# Count Responses of Quiz 1
count_resp(responses_ls$Quiz_1,
           id_regex = "[:digit:]+")

# Count Responses from All Quizzes
count_resp(responses_ls,
           id_regex = "[:digit:]+")
}
