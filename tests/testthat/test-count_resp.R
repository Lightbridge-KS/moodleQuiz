
responses_counted <-
count_resp.list(responses_ls,
                extract_id_from = "Email address",
                id_regex = "[:digit:]+")

responses_Quiz_1_counted <-
  count_resp.data.frame(responses_ls$Quiz_1,
                        extract_id_from = "Email address",
                        id_regex = "[:digit:]+")



# Count Resp: List ----------------------------------------------------------

test_that("count_resp.list() works", {
  # Check Class
  expect_s3_class(responses_counted, "tbl_df")
  # Check Column Names
  ## _Count_Resp_
  expect_regex_in_names(responses_counted, "Count_Resp")
  ## Quiz_x
  expect_regex_in_names(responses_counted, "Quiz_")
  ## State
  expect_regex_in_names(responses_counted, "State")
  ## Total
  expect_regex_in_names(responses_counted, "Total")
  # Check Column Type
  expect_type(responses_counted$Total_5, "integer")

})


# Count Resp: DF ----------------------------------------------------------

test_that("count_resp.data.frame() works", {
  # Check Class
  expect_s3_class(responses_Quiz_1_counted, "tbl_df")
  # Check Column Names
  ## Count_Resp_x
  expect_regex_in_names(responses_Quiz_1_counted, "Count_Resp")
  # Check Column Type
  expect_type(responses_Quiz_1_counted$Count_Resp_2, "integer")

})
