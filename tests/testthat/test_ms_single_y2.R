# Setup -------------------------------------------------------------------
library(dplyr)
library(testthat)
library(y2artisan)
library(officer)


text_settings <- list('result' = fp_text())
color_settings <- list('result' = 'black')

#### ms_single_y2 ####
context('ms_single_y2')


### Special symbols errors
test_that('Special symbols error, stops', {
  frequencies <- tibble(
    label = c('thing 1', 'x < 1'),
    value = c(1, 2),
    result = c(.5, .5)
  )

  expect_error(
    ms_single_y2(
      data = frequencies,
      label_color = color_settings,
      label_text = text_settings
    ),
    'mschart objects cannot contain the special symbols "&" or "<". Please remove those symbols from your data frame'
  )
})


test_that('Special symbols error, NAs give no error', {
  frequencies <- tibble(
    label = c('thing 1', NA_character_),
    value = c(1, NA_character_),
    result = c(.5, NA_real_)
  )

  chart <- ms_single_y2(
    data = frequencies,
    label_color = color_settings,
    label_text = text_settings
  )
  expect_error(chart, NA)
})


