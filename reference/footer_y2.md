# Create a string with question wording and n-size

Pipe in the dataset (responses) and specify variables. Save out the
nicely formatted string to an object to pass to the footer of an ms
chart.

## Usage

``` r
footer_y2(
  dataset,
  ...,
  q_type = "NULL",
  label_length = 100,
  prompt_rm = TRUE,
  after_symbol = "Select all that apply"
)
```

## Arguments

- dataset:

  Haven labelled dataset from which to pull the variable(s)

- ...:

  Unquoted variables

- q_type:

  DEFAULT = 'NULL'; When 'NULL', footer_y2() will automatically choose
  the variable type when standard Y2 naming conventions are used. When
  variables have non-standard names, accepts list of any of c('s', 'm',
  'oe', 'r', 'sl', 'md', 'n') used to specify question type. s = single
  select, m = multiple select, etc.

- label_length:

  DEFAULT = 100; Truncates question labels to specified length and adds
  a '...' where truncation has occurred.

- prompt_rm:

  DEFAULT TRUE; Attempts to better format the question by removing hard
  returns, white space, and anything after " - " or the input of the
  "after_symbol" argument. TRUE is needed in order to identify common
  stems.

- after_symbol:

  DEFAULT = 'Select all that apply'; Removes all question text after and
  including the provided symbol. Must match upper/lower case of question
  wording.

## Examples

``` r
if (FALSE) { # \dontrun{
responses %>%
  footer_y2(
   s_income
  )

mtcars %>%
  footer_y2(
   m_aware,
   prompt_rm = TRUE
  )
  } # }
```
