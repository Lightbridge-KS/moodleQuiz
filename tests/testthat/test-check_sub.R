
## Combine List to DF
grades_sub <-
  check_sub.list(grades_ls,
               extract_id_from = "Email address",
               id_regex = "[:digit:]+")

## Data Frame
grades_Quiz_1_sub <-
  check_sub.data.frame(grades_ls$Quiz_1,
                       extract_id_from = "Email address",
                       id_regex = "[:digit:]+")



# Check Sub: List -----------------------------------------------------------

test_that("check_sub.list() works", {
  # Returned class
  expect_s3_class(grades_sub, "tbl_df")
  # Check Column Names
  ## Has "Quiz_*"
  expect_regex_in_names(grades_sub, "^Quiz")
  ## Has "_State"
  expect_regex_in_names(grades_sub, "State$")
  ## Has "_Encode"
  expect_regex_in_names(grades_sub, "Encode$")
  ## Has "Total"
  expect_regex_in_names(grades_sub, "Total")

})

# Check Sub: DF -----------------------------------------------------------


test_that("check_sub.data.frame() works", {

  # Return Class
  expect_s3_class(grades_Quiz_1_sub, "tbl_df")
  # Check Column Names
  expect_named(grades_Quiz_1_sub,
               c("Name", "ID", "Institution", "Department",
                 "State", "Encode", "Started", "Grade_10.00",
                 paste0("Q", 1:8))
  )

})

