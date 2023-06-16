
# Test Errors, Warnings, Messages ----------------------------------------------

test_that("footer_y2 - error grouped x2", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = 2,
    m_var_3 = 3
  ) %>%
    dplyr::group_by(s_var, m_var_1)

  expect_error(
    footer_y2(dataset),
    'footer_y2 can currently only handle one grouping. Your data has multiple groups'
  )
})


test_that("footer_y2 - error NAs in grouping", {
  dataset <- tibble::tibble(
    s_var = c(1, 1),
    m_var_1 = c(1, NA),
    m_var_2 = c(1, 1),
    m_var_3 = c(1, 1)
  ) %>%
    dplyr::group_by(m_var_1)

  expect_error(
    footer_y2(dataset),
    'Grouping variable has missingness (NA\'s). Please manually filter out NA\'s or replace NA\'s with an explicit variable level.',
    fixed = TRUE
    )
})


test_that("footer_y2 - error var names", {
  dataset <- tibble::tibble(
    s_var = c(1, 1),
    q_var_1 = c(1, NA),
    m_var_2 = c(1, 1),
    m_var_3 = c(1, 1)
  )

  expect_error(
    footer_y2(dataset),
    "Variable names do not match standardized format. Please specify q_types for all questions.",
    fixed = TRUE
  )

  dataset <- tibble::tibble(
    s_var = c(1, 1),
    oe_var = c('one', 'two')
  )
  labelled::var_label(dataset$s_var) <- '1 or 1?'
  labelled::var_label(dataset$oe_var) <- 'Text this up'

  expect_message(
    footer_y2(dataset),
    "Note: Variable names match standardized format. Assuming question types.",
    fixed = TRUE
  )
})


test_that("footer_y2 - error not haven labelled", {
  dataset <- tibble::tibble(
    s_var = c(1, 1),
    q_var_1 = c(1, NA),
    m_var_2 = c(1, 1),
    m_var_3 = c(1, 1)
  )

  dataset <- tibble::tibble(
    s_var = c(1, 1),
    oe_var = c('one', 'two')
  )
  labelled::var_label(dataset$s_var) <- '1 or 1?'

  expect_error(
    footer_y2(dataset),
    'variable "oe_var" is not haven labelled, please use labelled data with this function.',
    fixed = TRUE
  )
})


test_that("footer_y2 - message on question types", {
  dataset <- tibble::tibble(
    s_var = c(1, 1, 1, 2, 3),
    m_var_1 = c(1, 1, 1, NA, 1),
    m_var_2 = c(1, 1, 1, NA, NA),
    m_var_3 = c(1, NA, 1, NA, NA),
    m_var_4_TEXT = c('yellow', 'lellow', 'green', '', 'NA'),
    oe_var = c('James', 'Jim', 'Jimmy', 'Santiago', '')
  )
  labelled::var_label(dataset$s_var) <- 'Marvel or DC?'
  labelled::var_label(dataset$m_var_1) <- 'Fav color? - Blue'
  labelled::var_label(dataset$m_var_2) <- 'Fav color? - Red'
  labelled::var_label(dataset$m_var_3) <- 'Fav color? - Cattleya'
  labelled::var_label(dataset$m_var_4_TEXT) <- 'Fav color? - please specify'
  labelled::var_label(dataset$oe_var) <- 'Your name:'

  expect_message(
    dataset %>% footer_y2(m_var_1),
    'Note: Variable names match standardized format. Assuming question types.'
  )
  expect_message(
    dataset %>% footer_y2(s_var, q_type = 's'),
    regexp = NA
  )
})



# Overall Tests -----------------------------------------------------------

test_that("footer_y2 - multi select questions", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = c(1, 1, 1, NA, 1),
    m_var_2 = c(1, 1, 1, NA, NA),
    m_var_3 = c(1, NA, 1, NA, NA),
    groupvar = c('G1', 'G1', 'G1', 'G2', 'G2')
  )
  labelled::var_label(dataset$m_var_1) <- 'Fav color? - Blue'
  labelled::var_label(dataset$m_var_2) <- 'Fav color? - Red'
  labelled::var_label(dataset$m_var_3) <- 'Fav color? - Cattleya'
  dataset_grouped <- dataset %>% dplyr::group_by(groupvar)

  expect_message(
    dataset %>% footer_y2(m_var_1),
    'Note: Stem "m_var" was used to find n size.'
  )
  expect_equal(
    dataset %>% footer_y2(m_var_1),
    'Q: Fav color? (n = 4)'
  )
  expect_equal(
    dataset_grouped %>% footer_y2(m_var_1),
    'Q: Fav color? n = (G1: 3, G2: 1)'
  )
})


test_that("footer_y2 - multiple variables", {
  dataset <- tibble::tibble(
    s_var = c(1, 1, 1, 2, 3),
    s_vary = c(1, 1, 1, 2, NA),
    m_var_1 = c(1, 1, 1, NA, 1),
    m_var_2 = c(1, 1, 1, NA, NA),
    m_var_3 = c(1, NA, 1, NA, NA),
    m_var2_1 = c(1, NA, 1, 1, NA),
    m_var2_2 = c(1, NA, 1, NA, 1),
    m_var_4_TEXT = c('yellow', 'lellow', 'green', '', 'NA'),
    oe_var = c('NA', 'Jim', 'Jimmy', 'Santiago', '')
  )
  labelled::var_label(dataset$s_var) <- 'Marvel or DC?'
  labelled::var_label(dataset$s_vary) <- 'Coke or Pepsi?'
  labelled::var_label(dataset$m_var_1) <- 'Fav color? - Blue'
  labelled::var_label(dataset$m_var_2) <- 'Fav color? - Red'
  labelled::var_label(dataset$m_var_3) <- 'Fav color? - Cattleya'
  labelled::var_label(dataset$m_var2_1) <- 'Fav shape? - Circle'
  labelled::var_label(dataset$m_var2_2) <- 'Fav shape? - Triskaidecagon'
  labelled::var_label(dataset$m_var_4_TEXT) <- 'Fav color? - please specify'
  labelled::var_label(dataset$oe_var) <- 'Your name:'

  expect_equal(
    dataset %>% footer_y2(m_var_1, s_var),
    'Q: Fav color? (n = 5)\nQ: Marvel or DC? (n = 5)'
  )
  expect_equal(
    dataset %>% footer_y2(m_var_1, oe_var),
    'Q: Fav color? (n = 5)\nQ: Your name: (n = 3)'
  )
  expect_equal(
    dataset %>% footer_y2(m_var_1, m_var2_1),
    'Q: Fav color? (n = 5)\nQ: Fav shape? (n = 4)'
  )
  expect_equal(
    dataset %>% footer_y2(s_vary, s_var),
    'Q: Coke or Pepsi? (n = 4)\nQ: Marvel or DC? (n = 5)'
  )
})


test_that("footer_y2 - whole data set", {
  dataset <- tibble::tibble(
    s_var = c(1, 1, 1, 2, 3),
    s_vary = c(1, 1, 1, 2, NA),
    m_var_1 = c(1, 1, 1, NA, 1),
    m_var_2 = c(1, 1, 1, NA, NA),
    m_var_3 = c(1, NA, 1, NA, NA),
    m_var2_1 = c(1, NA, 1, 1, NA),
    m_var2_2 = c(1, NA, 1, NA, 1),
    m_var_4_TEXT = c('yellow', 'lellow', 'green', '', 'NA'),
    oe_var = c('NA', 'Jim', 'Jimmy', 'Santiago', '')
  )
  labelled::var_label(dataset$s_var) <- 'Marvel or DC?'
  labelled::var_label(dataset$s_vary) <- 'Coke or Pepsi?'
  labelled::var_label(dataset$m_var_1) <- 'Fav color? - Blue'
  labelled::var_label(dataset$m_var_2) <- 'Fav color? - Red'
  labelled::var_label(dataset$m_var_3) <- 'Fav color? - Cattleya'
  labelled::var_label(dataset$m_var2_1) <- 'Fav shape? - Circle'
  labelled::var_label(dataset$m_var2_2) <- 'Fav shape? - Triskaidecagon'
  labelled::var_label(dataset$m_var_4_TEXT) <- 'Fav color? - please specify'
  labelled::var_label(dataset$oe_var) <- 'Your name:'

  expect_equal(
    footer_y2(dataset),
    'Q: Fav color? (n = 5)\nQ: Fav shape? (n = 4)\nQ: Marvel or DC? (n = 5)\nQ: Coke or Pepsi? (n = 4)\nQ: Fav color? (n = 3)\nQ: Your name: (n = 3)'
  )
})


test_that("footer_y2 - open ends", {
  dataset <- tibble::tibble(
    m_var_4_TEXT = c('yellow', 'lellow', 'green', '', 'NA'),
    oe_var = c('NA', 'Jim', 'Jimmy', 'Santiago', ''),
    groupvar = c('G1', 'G1', 'G1', 'G2', 'G2')
  )
  labelled::var_label(dataset$m_var_4_TEXT) <- 'Fav color? - please specify'
  labelled::var_label(dataset$oe_var) <- 'Your name:'
  dataset_grouped <- dataset %>% dplyr::group_by(groupvar)

  expect_equal(
    dataset %>% footer_y2(oe_var),
    'Q: Your name: (n = 3)'
  )
  expect_equal(
    dataset_grouped %>% footer_y2(oe_var),
    'Q: Your name: n = (G1: 2, G2: 1)'
  )
})


test_that("footer_y2 - multiple variables, grouped", {
  dataset <- tibble::tibble(
    s_var = c(1, 1, 1, 2, 3),
    s_vary = c(1, 1, 1, 2, NA),
    m_var_1 = c(1, 1, 1, NA, 1),
    m_var_2 = c(1, 1, 1, NA, NA),
    m_var_3 = c(1, NA, 1, NA, NA),
    m_var2_1 = c(1, NA, 1, 1, NA),
    m_var2_2 = c(1, NA, 1, NA, 1),
    m_var_4_TEXT = c('yellow', 'lellow', 'green', '', 'NA'),
    oe_var = c('NA', 'Jim', 'Jimmy', 'Santiago', ''),
    groupvar = c('G1', 'G1', 'G2', 'G2', 'G2')
  ) %>%
    dplyr::group_by(groupvar)
  labelled::var_label(dataset$s_var) <- 'Marvel or DC?'
  labelled::var_label(dataset$s_vary) <- 'Coke or Pepsi?'
  labelled::var_label(dataset$m_var_1) <- 'Fav color? - Blue'
  labelled::var_label(dataset$m_var_2) <- 'Fav color? - Red'
  labelled::var_label(dataset$m_var_3) <- 'Fav color? - Cattleya'
  labelled::var_label(dataset$m_var2_1) <- 'Fav shape? - Circle'
  labelled::var_label(dataset$m_var2_2) <- 'Fav shape? - Triskaidecagon'
  labelled::var_label(dataset$m_var_4_TEXT) <- 'Fav color? - please specify'
  labelled::var_label(dataset$oe_var) <- 'Your name:'
  labelled::var_label(dataset$groupvar) <- 'Groups'

  expect_equal(
    dataset %>% footer_y2(m_var_1, s_var),
    'Q: Fav color? n = (G1: 2, G2: 3)\nQ: Marvel or DC? n = (G1: 2, G2: 3)'
  )
  expect_equal(
    dataset %>% footer_y2(m_var_1, oe_var),
    'Q: Fav color? n = (G1: 2, G2: 3)\nQ: Your name: n = (G1: 1, G2: 2)'
  )
  expect_equal(
    dataset %>% footer_y2(m_var_1, m_var2_1),
    'Q: Fav color? n = (G1: 2, G2: 3)\nQ: Fav shape? n = (G1: 1, G2: 3)'
  )
  expect_equal(
    dataset %>% footer_y2(s_var, s_vary),
    'Q: Marvel or DC? n = (G1: 2, G2: 3)\nQ: Coke or Pepsi? n = (G1: 2, G2: 2)'
  )
})



# Argument Tests ----------------------------------------------------------

test_that("footer_y2 - argument q_type", {
  dataset <- tibble::tibble(
    q_var = c(1, 1),
    q_var2 = c('one', NA_character_)
  )
  labelled::var_label(dataset$q_var) <- '1 or 1?'
  labelled::var_label(dataset$q_var2) <- 'Text this up'

  footer_result <- footer_y2(
    dataset,
    q_type = c('s', 'oe')
  )

  expect_equal(
    footer_result,
    'Q: 1 or 1? (n = 2)\nQ: Text this up (n = 1)'
  )
})


test_that("footer_y2 - argument label_length", {
  dataset <- tibble::tibble(
    s_varx = c(1, 1),
    groups = c('Group 1', 'Group 2')
  )
  labelled::var_label(dataset$s_varx) <- 'how many characters show up in this test?'
  labelled::var_label(dataset$groups) <- 'groups'
  dataset_grouped <- dataset %>% dplyr::group_by(groups)

  footer_len5 <- footer_y2(
    dataset,
    s_varx,
    label_length = 5
  )
  footer_len5_grouped <- footer_y2(
    dataset_grouped,
    s_varx,
    label_length = 5
  )
  footer_len21 <- footer_y2(
    dataset,
    s_varx,
    label_length = 21
  )
  footer_len21_grouped <- footer_y2(
    dataset_grouped,
    s_varx,
    label_length = 21
  )

  expect_equal(
    footer_len5,
    'Q: ho... (n = 2)'
  )
  expect_equal(
    footer_len21,
    'Q: how many character... (n = 2)'
  )
  expect_equal(
    footer_len5_grouped,
    'Q: ho... n = (Gr...: 1, Gr...: 1)'
  )
  expect_equal(
    footer_len21_grouped,
    'Q: how many character... n = (Group 1: 1, Group 2: 1)'
  )
})


test_that("footer_y2 - argument after_symbol", {
  dataset <- tibble::tibble(
    s_varx = c(1, 1),
    s_varxx = c(NA, 1),
    s_vary = c(1, 2),
    s_varz = c(1, 3)
  )
  labelled::var_label(dataset$s_varx) <- 'how many characters show up in this test? Select all that apply.'
  labelled::var_label(dataset$s_varxx) <- 'how many characters show up in this test? select all that apply'
  labelled::var_label(dataset$s_vary) <- 'how many \n hard returns... Select all that apply'
  labelled::var_label(dataset$s_varz) <- 'how many characters show up in this test?'

  symbol_default <- footer_y2(
    dataset,
    s_varx
  )
  symbol_caps <- footer_y2(
    dataset,
    s_varxx
  )
  symbol_return <- footer_y2(
    dataset,
    s_vary
  )
  symbol_custom <- footer_y2(
    dataset,
    s_varz,
    after_symbol = 'char'
  )

  expect_equal(
    symbol_default,
    'Q: how many characters show up in this test? (n = 2)'
  )
  expect_equal(
    symbol_caps,
    'Q: how many characters show up in this test? select all that apply (n = 1)'
  )
  expect_equal(
    symbol_return,
    'Q: how many hard returns... (n = 2)'
  )
  expect_equal(
    symbol_custom,
    'Q: how many (n = 2)'
  )
})


test_that("footer_y2 - argument prompt_rm", {
  dataset <- tibble::tibble(
    s_varx = c(1, 1),
    s_vary = c(1, 2),
    s_varz = c(1, 3)
  )
  labelled::var_label(dataset$s_varx) <- 'test \n hard return '
  labelled::var_label(dataset$s_vary) <- 'test - hyphens'
  labelled::var_label(dataset$s_varz) <- 'test  a    bunch of spaces '

  prompt_return <- footer_y2(
    dataset,
    s_varx,
    prompt_rm = TRUE
  )
  prompt_hyphen <- footer_y2(
    dataset,
    s_vary,
    prompt_rm = TRUE
  )
  prompt_hyphen_F <- footer_y2(
    dataset,
    s_vary,
    prompt_rm = FALSE
  )
  prompt_spaces <- footer_y2(
    dataset,
    s_varz,
    prompt_rm = TRUE
  )

  expect_equal(
    prompt_return,
    'Q: test hard return (n = 2)'
  )
  expect_equal(
    prompt_hyphen,
    'Q: test (n = 2)'
  )
  expect_equal(
    prompt_hyphen_F,
    'Q: test - hyphens (n = 2)'
  )
  expect_equal(
    prompt_spaces,
    'Q: test a bunch of spaces (n = 2)'
  )
})

