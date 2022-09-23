## Regular Expression Table to validate report


report_col_regex <- list(
  moodle = c("Surname", "First name", "State"),
  grades = c("Surname", "First name", "State", "Grade", "Q\\."),
  responses = c("Surname", "First name", "State", "Response")
)

usethis::use_data(report_col_regex, overwrite = TRUE, internal = TRUE)
