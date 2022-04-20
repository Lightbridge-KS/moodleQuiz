


# Is some NYG -------------------------------------------------------------



test_that("is_some_grade_nyg() works", {

  df_nyg <- tibble::tibble(`Grade/xx` = c(1, 2, "Not yet graded"))
  df_graded <- tibble::tibble(`Grade/xx` = c(1, 2, 3))
  # `TRUE` if any  "Not yet graded", otherwise `FALSE`
  expect_true(is_some_grade_nyg(df_nyg))
  expect_false(is_some_grade_nyg(df_graded))
})


# Is some Numeric Grade ---------------------------------------------------


test_that("is_some_grade_numeric() works", {

  df_some_num <- tibble::tibble(`Grade/xx` = c("1.2", "1", "Not yet graded"))
  df_chr <- tibble::tibble(`Grade/xx` = c("hey", "Not yet graded"))
  # `TRUE` if some numeric grading, otherwise `FALSE`
  expect_true(is_some_grade_numeric(df_some_num))
  expect_false(is_some_grade_numeric(df_chr))

})
