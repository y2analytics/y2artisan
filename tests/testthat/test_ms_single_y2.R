
### Special symbols errors
test_that('Special symbols error, stops', {
  frequencies <- tibble::tibble(
    label = c('thing 1', 'x < 1'),
    value = c(1, 2),
    result = c(.5, .5)
  )
  text_settings <- list('result' = officer::fp_text())
  color_settings <- list('result' = 'black')

  expect_snapshot(error = TRUE,
    ms_single_y2(
      data = frequencies,
      label_color = color_settings,
      label_text = text_settings
    ))
})


test_that('Special symbols error, NAs give no error', {
  frequencies <- tibble::tibble(
    label = c('thing 1', NA_character_),
    value = c(1, NA_character_),
    result = c(.5, NA_real_)
  )
  text_settings <- list('result' = officer::fp_text())
  color_settings <- list('result' = 'black')

  chart <- ms_single_y2(
    data = frequencies,
    label_color = color_settings,
    label_text = text_settings
  )
  expect_no_error(chart)
})


