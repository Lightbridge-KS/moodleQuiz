
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moodleQuiz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Lightbridge-AI/moodleQuiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Lightbridge-AI/moodleQuiz/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Lightbridge-KS/moodleQuiz/branch/main/graph/badge.svg?token=S8SD1X5PA0)](https://codecov.io/gh/Lightbridge-KS/moodleQuiz)

<!-- badges: end -->

R package `{moodleQuiz}` contains high-level functions for cleaning,
encoding, filtering, and combining student’s score and responses from
[Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports).

# Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Lightbridge-KS/moodleQuiz")
```

# Overview

The 4 main functions of the `{moodleQuiz}` are as follows:

-   `check_sub()`: check student’s submission of the Moodle quiz report
    by looking at the `state` column.

-   `combine_resp()`: combine student’s responses (`Response x`) from
    the Responses report.

-   `count_resp()`: count how many responses each student answered from
    the Responses report.

-   `combine_grades()`: filter, adjust, and combine student’s grade from
    `Grade/xx` column.

All of theses 4 main functions are generic function that operate on
**data frame** and **list of data frames** of Moodle Quiz report. The
latter will be particularly useful when performing a data aggregation
across multiple Moodle quiz report of the same type.

# Example

``` r
library(moodleQuiz)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

Let’s say I have a Moodle Grades Report data frame of “Quiz_1”

``` r
glimpse(grades_ls$Quiz_1)
#> Rows: 27
#> Columns: 18
#> $ Surname         <chr> "Roquemore", "Ali", "Hoffpauir", "Babbitt", "Huynh", "…
#> $ `First name`    <chr> "Jada", "Ronin", "Jerry", "Nathan", "Rohith", "Joy", "…
#> $ Institution     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ Department      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ `Email address` <chr> "u017@example.com", "u001@example.com", "u002@example.…
#> $ State           <chr> "Finished", "Finished", "Finished", "Finished", "Finis…
#> $ `Started on`    <chr> "2 June 2021 8:28 AM", "2 June 2021 2:21 PM", "2 June …
#> $ `Time taken`    <chr> "16 mins 49 secs", "17 mins 11 secs", "11 mins 30 secs…
#> $ Completed       <chr> "2 June 2021 8:45 AM", "2 June 2021 2:38 PM", "2 June …
#> $ `Grade/10.00`   <dbl> 9.47, 9.74, 9.47, 10.00, 10.00, 10.00, 9.21, 9.47, 10.…
#> $ `Q. 1 /0.79`    <dbl> 0.26, 0.79, 0.26, 0.79, 0.79, 0.79, 0.79, 0.26, 0.79, …
#> $ `Q. 2 /0.26`    <dbl> 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, …
#> $ `Q. 3 /2.37`    <dbl> 2.37, 2.37, 2.37, 2.37, 2.37, 2.37, 2.11, 2.37, 2.37, …
#> $ `Q. 4 /2.11`    <dbl> 2.11, 2.11, 2.11, 2.11, 2.11, 2.11, 1.58, 2.11, 2.11, …
#> $ `Q. 5 /2.11`    <dbl> 2.11, 2.11, 2.11, 2.11, 2.11, 2.11, 2.11, 2.11, 2.11, …
#> $ `Q. 6 /0.26`    <dbl> 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, …
#> $ `Q. 7 /1.05`    <chr> "1.05", "0.79", "1.05", "1.05", "1.05", "1.05", "1.05"…
#> $ `Q. 8 /1.05`    <dbl> 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, …
```

**Combine & Adjust Grades of Quiz 1**

How to choose the maximum score of each student and readjust maximum
grades to 100?

Calling `combine_grades()` will do that in one step, plus it also cleans
column names and extracts numeric student ID from Email address.

``` r
grades_ls$Quiz_1 %>%
  combine_grades(
    extract_id_from = "Email address", # Extract Student ID from Email
    id_regex = "[:digit:]+", # Regular expression to extract student ID
    choose_grade = "max", # Choose only maximum grade of each student
    new_max_grade = 100 # Adjust maximum grade to 100
  ) %>%
  select(Name, ID, starts_with("G"), starts_with("Q"))
#> # A tibble: 26 × 11
#>    Name          ID    Grade_100    Q1    Q2    Q3    Q4    Q5    Q6    Q7    Q8
#>    <chr>         <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Jada Roquemo… 017        94.7   2.6   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  2 Ronin Ali     001        97.4   7.9   2.6  23.7  21.1  21.1   2.6   7.9  10.5
#>  3 Jerry Hoffpa… 002        94.7   2.6   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  4 Nathan Babbi… 026       100     7.9   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  5 Rohith Huynh  024       100     7.9   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  6 Joy Ellis     013       100     7.9   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  7 Jeremy Nelson 023        92.1   7.9   2.6  21.1  15.8  21.1   2.6  10.5  10.5
#>  8 Christopher … 003        94.7   2.6   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#>  9 Israel Munoz  018       100     7.9   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#> 10 Zubaida al-A… 022       100     7.9   2.6  23.7  21.1  21.1   2.6  10.5  10.5
#> # … with 16 more rows
```

**Combine Grades for All Quizzes**

Supposed in a semester student have to do a graded assignment in a
multiple quizzes, you can put each individual Grades Report in a list.

Now it’s **a list of data frames** (here as `grades_ls`).

``` r
grades_ls %>% 
  purrr::map_lgl(is.data.frame)
#> Quiz_1 Quiz_2 Quiz_3 
#>   TRUE   TRUE   TRUE
```

Supply a list of data frames into `combine_grades()`, you can *combine*
and *weight* grades from **multiple quizzes** into one data frame with a
sum of student’s score in the `Total_x` column.

``` r
grades_ls %>% 
  combine_grades(
    extract_id_from = "Email address", # Extract Student ID from Email
    id_regex = "[:digit:]+", # Regular expression to extract student ID
    choose_grade = "max", # Choose only maximum grade of each student
    new_max_grade = c(25, 25, 50) # Adjust maximum grade to 100
  ) %>% 
  select(Name, ID, contains("Grade"), starts_with("Total"))
#> # A tibble: 26 × 6
#>    Name          ID    Quiz_1_Grade_25 Quiz_2_Grade_25 Quiz_3_Grade_50 Total_100
#>    <chr>         <chr>           <dbl>           <dbl>           <dbl>     <dbl>
#>  1 Jada Roquemo… 017              23.7            19.4            50.0      93.1
#>  2 Ronin Ali     001              24.4            20.4            46.4      91.1
#>  3 Jerry Hoffpa… 002              23.7            16.6            50.0      90.3
#>  4 Nathan Babbi… 026              25              18.5            50.0      93.4
#>  5 Rohith Huynh  024              25              25              50.0     100. 
#>  6 Joy Ellis     013              25              13.9            50.0      88.8
#>  7 Jeremy Nelson 023              23.0            25              46.4      94.4
#>  8 Christopher … 003              23.7            22.2            50.0      95.8
#>  9 Israel Munoz  018              25              21.3            50.0      96.2
#> 10 Zubaida al-A… 022              25              22.2            50.0      97.2
#> # … with 16 more rows
```

# Learn more

-   [Get
    started](https://lightbridge-ks.github.io/moodleQuiz/articles/moodleQuiz.html)

------------------------------------------------------------------------

Last updated: 2022-04-25
