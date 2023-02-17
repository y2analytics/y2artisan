# Possible feature to add: all arguments have to be single
# Would be nice to allow vectors so each level can be different

test_that('set_text_settings_y2, error messages', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )

  expect_error(
    text_settings_fun <- set_text_settings_y2(
      dataset = frequencies,
      text_column = lable
    ),
    'Missing column "lable" in dataset'
  )
})


test_that('set_text_settings_y2, single', {
  frequencies <- data.frame(
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  text_settings_fun <- set_text_settings_y2(
    dataset = frequencies,
    single = TRUE
    )
  text_settings_man <- list(
    'result' = officer::fp_text(
      font.size = 14,
      color = 'white',
      font.family = 'BentonSans'
        )
  )

  expect_equal(text_settings_fun, text_settings_man)
})


test_that('set_text_settings_y2, stacked', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  text_settings_fun <- set_text_settings_y2(dataset = frequencies)
  text_settings_man <- list(
    'One' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans'),
    'Two' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans'),
    'Three' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans'),
    'Four' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans'),
    'Five' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans')
  )

  expect_equal(text_settings_fun, text_settings_man)
})


test_that('set_text_settings_y2, grouped', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  text_settings_fun <- set_text_settings_y2(
    dataset = frequencies,
    text_column = group_var
    )
  text_settings_man <- list(
    'group1' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans'),
    'group2' = officer::fp_text(font.size = 14, color = 'white',font.family = 'BentonSans')
  )

  expect_equal(text_settings_fun, text_settings_man)
})


test_that('set_text_settings_y2, font-color-family', {
  frequencies <- data.frame(
    group_var = c(rep('group1', 2), rep('group2', 3)),
    value = c(1:5),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = rep(.2, 5),
    n = rep(100, 5)
  )
  text_settings_fun <- set_text_settings_y2(
    dataset = frequencies,
    font_size = 12,
    font_color = 'black',
    font_family = 'Arial'
  )
  text_settings_man <- list(
    'One' = officer::fp_text(font.size = 12, color = 'black',font.family = 'Arial'),
    'Two' = officer::fp_text(font.size = 12, color = 'black',font.family = 'Arial'),
    'Three' = officer::fp_text(font.size = 12, color = 'black',font.family = 'Arial'),
    'Four' = officer::fp_text(font.size = 12, color = 'black',font.family = 'Arial'),
    'Five' = officer::fp_text(font.size = 12, color = 'black',font.family = 'Arial')
  )

  expect_equal(text_settings_fun, text_settings_man)
})

