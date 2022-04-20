

df_sim <- colnm_to_tbl(c("a", "b1", "c_"))

test_that("is_regex_in_names() works", {

  # All Regex in names
  is_regex_in_names(df_sim, regex = c("a", "b")) %>% expect_equal(c(T, T))
  # `d` is not in names
  is_regex_in_names(df_sim, regex = c("a", "d")) %>% expect_equal(c(T, F))

})
