
#' Convert column names to tibble
#'
#' @param nms Character vector
#'
#' @return A tibble with `nms` as column names
colnm_to_tbl <- function(nms){

  x <- seq_along(nms)
  names(x) <- nms
  tibble::as_tibble_row(x)

}


# Quiz Report
df_report_like <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Something"))
## Improper column format
df_report_notval <- colnm_to_tbl(c("Surname", "First_name", "Email", "State"))

# Grade Report
df_gr_rep <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Grade",
                            "Q. 1/1.00", "Q. 2/2.10"))
## Grade Report (Bad Q)
df_gr_rep_bad <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Grade",
                                "Q1", "Q2"))
# Response Report
df_resp <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Response 1", "Response 2"))
df_resp2 <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Response_1"))
df_resp_ra <- colnm_to_tbl(c("Surname", "First name", "Email address", "State", "Right answer 1", "Response 1"))


# Report Type -------------------------------------------------------------


test_that("get_report_type output test",{

  # Output
  expect_equal(get_report_type(grades_ls$Quiz_1), "Grades")
  expect_equal(get_report_type(responses_ls$Quiz_1), "Responses")
  expect_equal(get_report_type(iris), NA_character_)
  # Error Msg
  expect_error(get_report_type(list(iris)))

})

# Response Report ---------------------------------------------------------

test_that("is_responses_report() works",{

 # TRUE
 expect_true(is_responses_report(df_resp))
 expect_true(is_responses_report(responses_ls$Quiz_1))
 expect_true(is_responses_report(df_resp2)) # just has any column start with `Response`
 expect_true(is_responses_report(df_resp_ra)) # "Right answer" not interfere
 # FALSE
 expect_false(is_responses_report(df_gr_rep)) # Grade Report
 expect_false(is_responses_report(grades_ls$Quiz_1)) # Grade Report

})


# Grades Report -----------------------------------------------------------


test_that("is_grades_report() works", {

  # TRUE
  expect_true(is_grades_report(df_gr_rep))
  expect_true(is_grades_report(grades_ls$Quiz_1))
  # FALSE
  expect_false(is_grades_report(df_gr_rep_bad)) # Bad Grade Report
  expect_false(is_grades_report(df_resp)) # Response Report
  expect_false(is_grades_report(responses_ls$Quiz_1)) # Response Report

})


# Quiz Report -------------------------------------------------------------


test_that("is_report() works", {
  # TRUE
  expect_true(is_report(df_report_like)) # Fake
  expect_true(is_report(grades_ls$Quiz_1))
  expect_true(is_report(responses_ls$Quiz_1))
  # FALSE
  expect_false(is_report(df_report_notval))
})



