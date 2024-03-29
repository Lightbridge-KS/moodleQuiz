% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_quiz_type.R
\name{get_report_type}
\alias{get_report_type}
\title{Check Types of Moodle Quiz Report}
\usage{
get_report_type(data)
}
\arguments{
\item{data}{A data.frame to test.}
}
\value{
Character vector: "Grades" for Moodle Grades Report, "Responses" for Moodle Responses Report,
or \code{NA} if it's not a Moodle Quiz report
}
\description{
Check types of \href{https://docs.moodle.org/311/en/Quiz_reports}{Moodle Quiz report}
whether it is Grades or Responses report.
}
\examples{
# Grades Report
get_report_type(grades_ls$Quiz_1)
# Responses Report
get_report_type(responses_ls$Quiz_1)

}
