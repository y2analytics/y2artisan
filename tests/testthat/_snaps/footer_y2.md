# footer_y2 - error grouped x2

    Code
      footer_y2(dataset)
    Condition
      Error in `footer_y2()`:
      ! footer_y2 can currently only handle one grouping. Your data has multiple groups

# footer_y2 - error NAs in grouping

    Code
      footer_y2(dataset)
    Condition
      Error in `footer_y2()`:
      ! Grouping variable has missingness (NA's). Please manually filter out NA's or replace NA's with an explicit variable level.

# footer_y2 - error var names

    Code
      footer_y2(dataset)
    Condition
      Error in `footer_y2()`:
      ! Variable names do not match standardized format. Please specify q_types for all questions.

---

    Code
      footer_y2(dataset)
    Message
      Note: Variable names match standardized format. Assuming question types.
    Output
      [1] "Q: 1 or 1? (n = 2)\nQ: Text this up (n = 2)"

# footer_y2 - error not haven labelled

    Code
      footer_y2(dataset)
    Message
      Note: Variable names match standardized format. Assuming question types.
    Condition
      Error in `footer_y2()`:
      ! variable "oe_var" is not haven labelled, please use labelled data with this function.

# footer_y2 - message on question types

    Code
      dataset %>% footer_y2(m_var_1)
    Message
      Note: Variable names match standardized format. Assuming question types.
      Note: Stem "m_var" was used to find n size.
    Output
      [1] "Q: Fav color? (n = 4)"

---

    Code
      dataset %>% footer_y2(s_var, q_type = "s")
    Output
      [1] "Q: Marvel or DC? (n = 5)"

# footer_y2 - multi select questions

    Code
      dataset %>% footer_y2(m_var_1)
    Message
      Note: Variable names match standardized format. Assuming question types.
      Note: Stem "m_var" was used to find n size.
    Output
      [1] "Q: Fav color? (n = 4)"

