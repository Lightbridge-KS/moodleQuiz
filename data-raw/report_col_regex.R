## code to prepare `report_col_regex` dataset goes here

usethis::use_data(report_col_regex, overwrite = TRUE)


report_col_regex <- list(
  moodle = c("Surname", "First name", "Email address", "State"),
  grades = c("Surname", "First name", "Email address", "State", "Grade", "Q"),
  responses = c("Surname", "First name", "Email address", "State", "Response")
)
