Loading required package: RMySQL
Loading required package: DBI
Loading required package: methods
Loading required package: fst
Loading required package: data.table
Loading required package: tidyverse
── Attaching packages ─────────────────────────────────────── tidyverse 1.2.1 ──
✔ ggplot2 2.2.1     ✔ purrr   0.2.4
✔ tibble  1.4.2     ✔ dplyr   0.7.4
✔ tidyr   0.8.0     ✔ stringr 1.2.0
✔ readr   1.1.1     ✔ forcats 0.3.0
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::between()   masks data.table::between()
✖ dplyr::filter()    masks stats::filter()
✖ dplyr::first()     masks data.table::first()
✖ dplyr::lag()       masks stats::lag()
✖ dplyr::last()      masks data.table::last()
✖ purrr::transpose() masks data.table::transpose()
Loading required package: stringi
Parsed with column specification:
cols(
  u = col_character(),
  pw = col_character()
)
Parsed with column specification:
cols(
  u = col_character(),
  pw = col_character()
)
Warning message:
In .local(conn, statement, ...) :
  Decimal MySQL column 1 imported as numeric
Warning message:
In .local(conn, statement, ...) :
  Decimal MySQL column 1 imported as numeric
Parsed with column specification:
cols(
  u = col_character(),
  pw = col_character()
)
Parsed with column specification:
cols(
  u = col_character(),
  pw = col_character()
)
Warning message:
In `[.data.table`(data.table(tmp), , lapply(.SD, na.omit), by = drug_concept_id) :
  Column 4 of result for group 150 is length 2 but the longest column in this result is 3. Recycled leaving remainder of 1 items. This warning is once only for the first group with this issue.
Joining, by = "drug_concept_id"
Joining, by = "outcome_concept_id"
Warning messages:
1: In .local(conn, statement, ...) :
  Decimal MySQL column 0 imported as numeric
2: In .local(conn, statement, ...) :
  Decimal MySQL column 1 imported as numeric
3: In .local(conn, statement, ...) :
  Decimal MySQL column 11 imported as numeric
4: In .local(conn, statement, ...) :
  Decimal MySQL column 15 imported as numeric
