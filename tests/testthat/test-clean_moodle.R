

# Clean Data --------------------------------------------------------------


## Grades
grades_Quiz_1_cleaned <-
  clean_moodle(grades_ls$Quiz_1,
               extract_id_from = "Email address",
               id_regex = "[:digit:]+")
## Responses
responses_Quiz_1_cleaned <-
clean_moodle(responses_ls$Quiz_1,
             extract_id_from = "Email address",
             id_regex = "[:digit:]+")


# Clean: Grade Report -----------------------------------------------------

test_that("clean_moodle() on `grades_ls$Quiz_1`, columns names works", {

  # Returned Class
  expect_s3_class(grades_Quiz_1_cleaned, "tbl_df")
  # Check column names
  expect_named(grades_Quiz_1_cleaned,
               c("Name", "ID", "Institution", "Department", "State", "Started", "Grade_10.00",
                 paste0("Q", 1:8))
  )
  # Class of Columns
  ## "Started" is POSIXct
  expect_s3_class(grades_Quiz_1_cleaned$Started, "POSIXct")
  ## Numeric columns "Grade_10.00" and "Qx"
  grades_Quiz_1_num_cols <- grades_Quiz_1_cleaned %>% purrr::keep(is.numeric)
  expect_named(grades_Quiz_1_num_cols,
               c("Grade_10.00", paste0("Q", 1:8)))

})


# Clean: Response Report --------------------------------------------------

test_that("clean_moodle() on `responses_ls$Quiz_1`, columns names works", {

  # Returned Class
  expect_s3_class(responses_Quiz_1_cleaned, "tbl_df")
  # Check column names
  expect_named(responses_Quiz_1_cleaned,
               c("Name", "ID", "Institution", "Department", "State", "Started", "Grade_2.00",
                 paste0("Response_",1:2)))

  # Class of Columns
  ## "Started" is POSIXct
  expect_s3_class(responses_Quiz_1_cleaned$Started, "POSIXct")
  ## Numeric columns "Grade_2.00"
  responses_Quiz_1_num_cols <- responses_Quiz_1_cleaned %>% purrr::keep(is.numeric)
  expect_named(responses_Quiz_1_num_cols, "Grade_2.00")

})
