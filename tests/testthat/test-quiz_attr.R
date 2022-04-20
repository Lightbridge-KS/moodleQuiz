

# Quiz Attributes ---------------------------------------------------------


test_that("get_quiz_attr",{

  # Grades
  grade_nm <- c("report_type", "some_nyg", "grade_max", "q_no", "q_max")
  get_quiz_attr(grades_ls$Quiz_1) %>% expect_named(grade_nm)

  # Responses
  resp_nm <- c("report_type", "some_nyg", "grade_max",  "resp_no", "cloze_cols")
  get_quiz_attr(responses_ls$Quiz_1) %>% expect_named(resp_nm)

  # Error
  get_quiz_attr(grades_ls) %>% expect_error("`data` must be a data.frame")
  get_quiz_attr(iris) %>% expect_error("is not a moodle quiz report")

})


# Get Maximum Grade -------------------------------------------------------



test_that("get_max_grade() works", {

  # Regex: one or more digit, follow by dot, then one or more digit

  df_gr1 <- colnm_to_tbl(c("Grade/12.34")) # Default of Moodle
  df_gr2 <- colnm_to_tbl(c("Grade_10.00"))
  df_gr3 <- colnm_to_tbl(c("Grade_1")) # Will not extract

  # Extract any digits
  expect_equal(get_max_grade(df_gr1), 12.34)
  expect_equal(get_max_grade(df_gr2), 10)
  expect_equal(get_max_grade(df_gr3), NA_integer_)

})

# Get Question Number and Max Score ---------------------------------------


test_that("get_questions_no_max() works", {

  df_q_test <- colnm_to_tbl(c("Q. 1/1.00", "Q. 2 /2.10", "Q.3/ 4.30"))

  res_df <- get_questions_no_max(df_q_test)
  # Check Class
  expect_s3_class(res_df, "data.frame")
  # Check Question Number
  expect_equal(res_df$q_no, 1:3)
  # Check Question Maximum
  expect_equal(res_df$q_max, c(1, 2.1, 4.3))
})


# Get Response Number -----------------------------------------------------


test_that("get_responses_no() works", {

  # Response x
  df_resp_simple <- colnm_to_tbl(paste0("Response ", 1:3))

  expect_equal(get_responses_no(df_resp_simple), 1:3)

})
