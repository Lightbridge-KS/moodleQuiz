% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quiz_attr.R
\name{get_quiz_attr}
\alias{get_quiz_attr}
\title{Get Moodle Quiz Attributes}
\usage{
get_quiz_attr(data)
}
\arguments{
\item{data}{A data.frame of \href{https://docs.moodle.org/311/en/Quiz_reports}{Moodle Quiz report}.}
}
\value{
A List containing Quiz attributes such as: \itemize{
\item \strong{\code{report_type}}: (character) "Grades" for Moodle Grade report or "Responses" for Moodle Responses report.
\item \strong{\code{some_nyg}}: (logical) \code{TRUE}: If "Grade/xx" column of the Moodle Quiz report contained some grades that are "Not yet graded".
\code{FALSE}: If "Grade/xx" column contained all numeric grades.
\item \strong{\code{grade_max}}: (numeric) Maximum grade of the Quiz
\item \strong{\code{q_no}}: (numeric) Only if \code{report_type} is "Grades", then \code{q_no} shows questions number presented in the Moodle Grades report.
\item \strong{\code{q_max}}: (numeric) Only if \code{report_type} is "Grades", then \code{q_max} shows maximum scores of corresponding \code{q_no}.
\item \strong{\code{resp_no}}: (numeric)  Only if \code{report_type} is "Responses", then \code{resp_no} shows responses number of the Moodle Responses report.
\item \strong{\code{cloze_cols}}: (character) Only if \code{report_type} is "Responses", then \code{cloze_cols} shows names of the "Responses" column that contained embedded answer (Cloze).
If no Cloze column is presented, return \code{NULL}.
}
}
\description{
Get attributes or meta-information from \href{https://docs.moodle.org/311/en/Quiz_reports}{Moodle Quiz report} (i.e., Grades or Responses report)
such as type of Moodle Quiz report, maximum grade of each quiz, question's number, question's maximum score,
or embedded answer (Cloze) column names (if present).
}
\examples{
# Grades Report
get_quiz_attr(grades_ls$Quiz_1)

# Responses Report
get_quiz_attr(responses_ls$Quiz_1)
}
