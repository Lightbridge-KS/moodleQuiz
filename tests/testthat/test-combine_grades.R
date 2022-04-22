
grades_comb_gr <- combine_grades.list(grades_ls)

grades_Quiz_1_adj <-
  adj_grades_moodle(grades_Quiz_1_cleaned,
                    new_max_grade = 100)



# Combine Grade: List -----------------------------------------------------

test_that("combine_grades.list() works", {

  # Check Class
  expect_s3_class(grades_comb_gr, "tbl_df")
  # Check Column Names
  expect_regex_in_names(grades_comb_gr, "Grade")
  expect_regex_in_names(grades_comb_gr, "_State")
  expect_regex_in_names(grades_comb_gr, "Quiz_")

  # Check Column type
  expect_type(grades_comb_gr$Total_30, "double")

})


# Adjust Grade ------------------------------------------------------------



test_that("adj_grades_moodle() works",{
  # Check Class
  expect_s3_class(grades_Quiz_1_adj, "tbl_df")
  # Check Adjusted Grade
  expect_equal(grades_Quiz_1_cleaned$Grade_10.00 * 10,
               grades_Quiz_1_adj$Grade_100,
               tolerance = 0.001)


})
