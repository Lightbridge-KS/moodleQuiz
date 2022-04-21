# Clean Data --------------------------------------------------------------


## Grades (single attempt)
grades_Quiz_1_cleaned <-
  clean_moodle(grades_ls$Quiz_1,
               extract_id_from = "Email address",
               id_regex = "[:digit:]+")
## Responses (single attempt)
responses_Quiz_1_cleaned <-
  clean_moodle(responses_ls$Quiz_1,
               extract_id_from = "Email address",
               id_regex = "[:digit:]+")


# Encode State ------------------------------------------------------------

## Grades (single attempt)
grades_Quiz_1_encoded <-
  encode_moodle(grades_Quiz_1_cleaned)

## Responses (single attempt)
responses_Quiz_1_encoded <-
  encode_moodle(responses_Quiz_1_cleaned)
