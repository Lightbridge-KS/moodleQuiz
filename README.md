
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moodleQuiz

<!-- badges: start -->
<!-- badges: end -->

The goal of the `moodleQuiz` package is to perform data wrangling,
merging and analyse [Moodle Quiz
report](https://docs.moodle.org/311/en/Quiz_reports) with simple
ready-to-use functions.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Lightbridge-KS/moodleQuiz")
```

## Combine Student Grades

This is an example of Moodle Grades report:

``` r
library(moodleQuiz)
data("grades_ls")

head(grades_ls$Quiz_1)
#>     Surname First name Institution Department    Email address    State
#> 1 Roquemore       Jada          NA         NA u017@example.com Finished
#> 2       Ali      Ronin          NA         NA u001@example.com Finished
#> 3 Hoffpauir      Jerry          NA         NA u002@example.com Finished
#> 4   Babbitt     Nathan          NA         NA u026@example.com Finished
#> 5     Huynh     Rohith          NA         NA u024@example.com Finished
#> 6     Ellis        Joy          NA         NA u013@example.com Finished
#>            Started on      Time taken           Completed Grade/10.00
#> 1 2 June 2021 8:28 AM 16 mins 49 secs 2 June 2021 8:45 AM        9.47
#> 2 2 June 2021 2:21 PM 17 mins 11 secs 2 June 2021 2:38 PM        9.74
#> 3 2 June 2021 4:04 PM 11 mins 30 secs 2 June 2021 4:16 PM        9.47
#> 4 2 June 2021 0:00 AM 11 mins 50 secs 2 June 2021 0:12 AM       10.00
#> 5 2 June 2021 2:47 PM 14 mins 46 secs 2 June 2021 3:02 PM       10.00
#> 6 2 June 2021 3:56 AM 10 mins 34 secs 2 June 2021 4:06 AM       10.00
#>   Q. 1 /0.79 Q. 2 /0.26 Q. 3 /2.37 Q. 4 /2.11 Q. 5 /2.11 Q. 6 /0.26 Q. 7 /1.05
#> 1       0.26       0.26       2.37       2.11       2.11       0.26       1.05
#> 2       0.79       0.26       2.37       2.11       2.11       0.26       0.79
#> 3       0.26       0.26       2.37       2.11       2.11       0.26       1.05
#> 4       0.79       0.26       2.37       2.11       2.11       0.26       1.05
#> 5       0.79       0.26       2.37       2.11       2.11       0.26       1.05
#> 6       0.79       0.26       2.37       2.11       2.11       0.26       1.05
#>   Q. 8 /1.05
#> 1       1.05
#> 2       1.05
#> 3       1.05
#> 4       1.05
#> 5       1.05
#> 6       1.05
```

Suppose that you have multiple reports, you can combine them in to a
list of data frame

``` r
# the example data "grades_ls" is a named list of data frame.
class(grades_ls)
#> [1] "list"
purrr::map(grades_ls, class)
#> $Quiz_1
#> [1] "tbl_df"     "tbl"        "data.frame"
#> 
#> $Quiz_2
#> [1] "tbl_df"     "tbl"        "data.frame"
#> 
#> $Quiz_3
#> [1] "tbl_df"     "tbl"        "data.frame"
```

Then, you can combine multiple Grades report to a single data frame
with: `combine_grades()`

``` r
moodleQuiz::combine_grades(grades_ls)
#> # A tibble: 26 × 9
#>    Name       ID       Quiz_1_State Quiz_1_Grade_10 Quiz_2_State Quiz_2_Grade_10
#>    <chr>      <chr>    <chr>                  <dbl> <chr>                  <dbl>
#>  1 Jada Roqu… u017@ex… Finished                9.47 Finished                7.77
#>  2 Ronin Ali  u001@ex… Finished                9.74 Finished                8.14
#>  3 Jerry Hof… u002@ex… Finished                9.47 Finished                6.66
#>  4 Nathan Ba… u026@ex… Finished               10    Finished                7.4 
#>  5 Rohith Hu… u024@ex… Finished               10    Finished               10   
#>  6 Joy Ellis  u013@ex… Finished               10    Finished                5.55
#>  7 Jeremy Ne… u023@ex… Finished                9.21 Finished               10   
#>  8 Christoph… u003@ex… Finished                9.47 Finished                8.88
#>  9 Israel Mu… u018@ex… Finished               10    Finished                8.51
#> 10 Zubaida a… u022@ex… Finished               10    Finished                8.88
#> # … with 16 more rows, and 3 more variables: Quiz_3_State <chr>,
#> #   Quiz_3_Grade_10 <dbl>, Total_30 <dbl>
```
