# moodleQuiz 0.1.1

-   Remove `Email address` column from the requirement to be a Moodle Quiz report.

# moodleQuiz 0.1.0 🎓 🎉

A high-level data manipulation R package 📦 for cleaning, encoding, filtering, and combining student's score and responses from [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports).

## Main Functions

4 main functions are:

-   [`check_sub()`](https://lightbridge-ks.github.io/moodleQuiz/reference/check_sub.html): check student's submission of the Moodle quiz report by looking at the `state` column.
-   [`combine_resp()`](https://lightbridge-ks.github.io/moodleQuiz/reference/combine_resp.html): combine student's responses (`Response x`) from the Responses report.
-   [`count_resp()`](https://lightbridge-ks.github.io/moodleQuiz/reference/count_resp.html): count how many responses each student answered from the Responses report.
-   [`combine_grades()`](https://lightbridge-ks.github.io/moodleQuiz/reference/combine_grades.html): filter, adjust, and combine student's grade from `Grade/xx` column.

## Quiz Attributes

-   [`get_quiz_attr()`](https://lightbridge-ks.github.io/moodleQuiz/reference/get_quiz_attr.html): to get meta-information from [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports)

## Helpers

-   [`clean_moodle()`](https://lightbridge-ks.github.io/moodleQuiz/reference/clean_moodle.html): clean Moodle Quiz report

-   [`get_report_type()`](https://lightbridge-ks.github.io/moodleQuiz/reference/get_report_type.html): check types of [Moodle Quiz report](https://docs.moodle.org/311/en/Quiz_reports) whether it is Grades or Responses report.

## Example Data

-   [`grades_ls`](https://lightbridge-ks.github.io/moodleQuiz/reference/grades_ls.html): Moodle Grades Report Example

-   [`responses_ls`](https://lightbridge-ks.github.io/moodleQuiz/reference/responses_ls.html): Moodle Responses Report Example

# moodleQuiz 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
