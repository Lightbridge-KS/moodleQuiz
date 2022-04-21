


# Encode Moodle -----------------------------------------------------------



test_that("encode_moodle() works", {

  # Has "Encode" column
  expect_regex_in_names(grades_Quiz_1_encoded, "Encode") # Grade
  expect_regex_in_names(responses_Quiz_1_encoded, "Encode") # Response

  # Finished encode as 1
  expect_equal(unique(grades_Quiz_1_encoded$Encode), 1)
  expect_equal(unique(responses_Quiz_1_encoded$Encode), 1)

})




# Encoder -----------------------------------------------------------------


test_that("encoder() works", {

  res <- encoder(c("a","b","d"), c("b","a","c"), c("B", "A", "C"))
  expect_identical(res, c("A", "B", NA))

})


