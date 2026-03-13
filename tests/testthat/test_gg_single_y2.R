
#### Errors when changing the defaults? ####
### Overall
test_that('frequencies', {
  frequencies <- mtcars %>% y2clerk::freqs(gear)
    chart <- gg_single_y2(frequencies, font_family = 'sans')
  vdiffr::expect_doppelganger('frequencies', chart)
})


### Overall
test_that('all defaults', {
  chart <- tibble::tibble(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_single_y2(font_family = 'sans')
  vdiffr::expect_doppelganger('all defaults', chart)
})


### Variables
# x_var
test_that('x_var', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    value = c(1:5),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(x_var = value, font_family = 'sans')
  vdiffr::expect_doppelganger('using x_var', chart)
})
# y_var
test_that('y_var', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    test = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(y_var = test, font_family = 'sans')
  vdiffr::expect_doppelganger('using y_var', chart)
})
# label_var
test_that('label_var', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    test = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(label_var = test, font_family = 'sans')
  vdiffr::expect_doppelganger('using label_var', chart)
})
# color_var
test_that('color_var', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    colors = c(1, 1, 1, 2, 2) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      color_var = colors,
      fills = c('#474E7E', '#A3A7BF'),
      font_family = 'sans'
      )
  vdiffr::expect_doppelganger('using color_var',chart)
})


### Axis
# axis_title_size & axis_text_size
test_that('axis text', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      axis_text_size = 20,
      axis_title_size = 20,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using_axis text', chart)
})
# y_min & y_max
test_that('mins and maxes', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      y_min = -.1,
      y_max = 2,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using mins and maxes', chart)
})
# x_label & y_label
test_that('axis labels', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      x_label = 'X axis label',
      y_label = 'Y axis label',
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using axis labels', chart)
})


### Bars
# bar_width
test_that('bar width', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      bar_width = 1,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using bar width', chart)
})
# direction
test_that('direction', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      direction = 'horizontal',
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using direction', chart)
})
# nudge
test_that('nudge', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      nudge = .1,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using nudge', chart)
})


### Legend
# legend_pos & legend_rev
test_that('legend_pos & legend_rev', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      legend_pos = 'top',
      legend_rev = T,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using legend_pos and legend_rev', chart)
})
# legend_text_size & legend_title_size
test_that('legend size', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      legend_text_size = 8,
      legend_title_size = 10,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using legend_size', chart)
})
# legend_title
test_that('legend_title', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      legend_title = 'Legend title',
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using legend_title', chart)
})


### Labels
# label_length & label_size
test_that('label_length & label_size', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      label_length = 10,
      label_size = 5,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using label_length & label_size', chart)
})


### Title
# title_label & title_size
test_that('title_label & title_size', {
  chart <- data.frame(
    label = c(1:5) %>% as.character(),
    result = rep(.2, 5),
    percent_label = stringr::str_c(c(1:5), '%')
  ) %>%
    gg_single_y2(
      title_label = 'My title',
      title_size = 20,
      font_family = 'sans'
    )
  vdiffr::expect_doppelganger('using title_label & title_size', chart)
})



