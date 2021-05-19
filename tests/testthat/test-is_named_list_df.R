context("is_named_list_data.frame")

test_that("is_named_list_data.frame output is correct",{

  nl <- list(m = mtcars, m = iris)
  l <- list(mtcars, iris)
  df <- iris
  vct <- 1:10
  # Test Output
  expect_true(is_named_list_data.frame(nl))
  expect_false(is_named_list_data.frame(l))
  expect_false(is_named_list_data.frame(df))
  expect_false(is_named_list_data.frame(vct))
  # Message
  expect_message(is_named_list_data.frame(l), "is list but has no names")
  expect_message(is_named_list_data.frame(df), "is a data.frame, not a list of data.frame")
  expect_message(is_named_list_data.frame(vct), "is not a list")

})
