


responses_comb <-
  combine_resp.list(responses_ls,
                    extract_id_from = "Email address",
                    id_regex = "[:digit:]+")


# Combine Resp: List ------------------------------------------------------


test_that("combine_resp.list() works", {
  # Check class
  expect_s3_class(responses_comb, "tbl_df")
  # Check Column Names
  ## Quiz_x
  expect_regex_in_names(responses_comb, "^Quiz_\\d+")
  ## State
  expect_regex_in_names(responses_comb, "State")
  ## _Response_x
  expect_regex_in_names(responses_comb, "Response_\\d+")

})
