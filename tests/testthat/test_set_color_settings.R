# Possible feature to add: all arguments have to be single
# Would be nice to allow vectors so each level can be different

test_that('set_color_settings_y2, error messages', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )


  expect_error(
    color_settings_fun <- set_color_settings_y2(
      dataset = frequencies,
      color_column = lable
    ),
    'Missing column "lable" in dataset'
  )


  expect_error(
    color_settings_fun <- set_color_settings_y2(
      dataset = frequencies,
      'blue'
    ),
    'Not enough colors provided. Please provide 4 more color(s) OR check the "label" column in the data.',
    fixed = TRUE
  )


  expect_error(
    color_settings_fun <- set_color_settings_y2(
      dataset = frequencies,
      color_column = group_var,
      'blue',
      'yellow',
      'red'
    ),
    'Too many colors provided. Please provide 1 less color(s) OR check the \"group_var\" column in the data.',
    fixed = TRUE
  )

})


test_that('set_color_settings_y2, single', {
  frequencies <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  color_settings_fun <- set_color_settings_y2(
    dataset = frequencies,
    single = TRUE,
    'blue'
  )
  color_settings_man <- 'blue'

  expect_equal(color_settings_fun, color_settings_man)
})


test_that('set_color_settings_y2, stacked', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  color_settings_fun <- set_color_settings_y2(
    dataset = frequencies,
    'black',
    'purple',
    'blue',
    'green',
    'white'
    )
  color_settings_man <- list(
    'One' =   'black',
    'Two' =   'purple',
    'Three' = 'blue',
    'Four' =  'green',
    'Five' =  'white'
  ) %>%
    purrr::as_vector()

  expect_equal(color_settings_fun, color_settings_man)
})


test_that('set_color_settings_y2, grouped', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  color_settings_fun <- set_color_settings_y2(
    dataset = frequencies,
    color_column = group_var,
    'black',
    'purple'
  )
  color_settings_man <- list(
    'group1' =   'black',
    'group2' =   'purple'
  ) %>%
    purrr::as_vector()

  expect_equal(color_settings_fun, color_settings_man)
})

