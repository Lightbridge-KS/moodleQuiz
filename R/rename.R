#' Prefix or Suffix column names in a list of Data frame with list names
#'
#' Add names of list of data frame to prefix or suffix specified column names in data frames in a list of data frame.
#'
#' @param ls_df A list of data.frame
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to rename, this column must presented in all data.frame
#' @param name_position (Character) "prefix": prefix names to columns or "suffix": suffix names to columns
#' @param sep (Character) Indicate separation of names of list of data.frame and `.cols`
#'
#' @return A list of data.frame with renamed columns
#'
rename_with_ls_df_names <- function(ls_df,
                                    .cols,
                                    name_position = c("prefix", "suffix"),
                                    sep = "_"
){

  name_position <- match.arg(name_position)
  nm <- names(ls_df)
  ls <- vector("list", length(ls_df))

  name_fun <- switch (name_position,
                      "prefix" = { ~paste0(nm[[i]], sep ,.x)  },
                      "suffix" = { ~paste0(.x, sep , nm[[i]]) }
  )

  for (i in seq_along(nm)) {

    ls[[i]] <- dplyr::rename_with(ls_df[[i]], .cols = {{.cols}}, name_fun)

  }

  names(ls) <- nm
  ls

}
