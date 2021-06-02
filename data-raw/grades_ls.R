## Simulate Grades Data

library(dplyr)
library(purrr)
library(readr)


# Read real Moodle Grades report data -------------------------------------

tech_grad_names <- fs::dir_ls("my_data/Grades", glob = "*Techno*_num*") %>%
  fs::path_file() %>%
  fs::path_ext_remove()

tech_grad_ls_raw <- fs::dir_ls("my_data/Grades", glob = "*Techno*_num*") %>%
  map(read_csv) %>%
  setNames(tech_grad_names)


# Select only Grade and Q... columns -----------------------------------------

tech_StateGradeQ <- tech_grad_ls_raw %>%
  map(~select(.x, State, starts_with("Grade"), starts_with("Q")))


# Simulate Demographic Data (n = 26) -----------------------------------------------

set.seed(123)
tech_sim1_demo <- simulate_moodle_demo(26)



# Simulate Time Data (n = 26) ------------------------------------------------------


set.seed(123)

## Time Data (not the same in 3 times)
tech_sim1_time_ls <- vector("list", 3)
for (i in 1:3) {

  tech_sim1_time_ls[[i]] <- random_time_moodle_df(26, time_taken = c(10,20))

}


# Combine  ----------------------------------------------------------------


grades_ls <- tech_sim1_time_ls %>%
  map(~bind_cols(tech_sim1_demo, .x)) %>%
  # Shuffle all Rows
  map(~slice_sample(.x, prop = 1)) %>%
  # Add Overall average at last Row
  map(~bind_rows(.x, tibble(Surname = "Overall average"))) %>%
  # Combine To Real Grades Report Data
  map2(.y = tech_StateGradeQ,
       ~bind_cols(.x, .y)
  ) %>%
  map(~relocate(.x, State, .after = "Email address")) %>%
  setNames(paste0("Quiz_", 1:3))


usethis::use_data(grades_ls, overwrite = TRUE)



