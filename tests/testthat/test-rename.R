

iris_ls_df <- list(iris_1 = iris, iris_2 = iris)

# Prefix `Sepal` with Names of the List
nms_ls1 <- rename_with_ls_df_names(
  iris_ls_df,
  tidyselect::starts_with("Sepal")
) %>%
  purrr::map(names)


test_that("rename_with_ls_df_names() works", {


  expect_identical(
    nms_ls1$iris_1,
    c(
      "iris_1_Sepal.Length", "iris_1_Sepal.Width",
      "Petal.Length", "Petal.Width", "Species"
    )
  )

  expect_identical(
    nms_ls1$iris_2,
    c(
      "iris_2_Sepal.Length", "iris_2_Sepal.Width",
      "Petal.Length", "Petal.Width", "Species"
    )
  )
})
