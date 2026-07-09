
#### Errors when changing the defaults? ####
test_that('all defaults', {
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    result = rep(.2, 10),
    group_var = c(1,1,1,1,1,2,2,2,2,2),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_grouped_y2(font_family = 'sans')
  expect_snapshot(error = TRUE, chart)
})


test_that('x_var', {
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    value = label %>% as.numeric,
    result = rep(.2, 10),
    group_var = c(1,1,1,1,1,2,2,2,2,2),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_grouped_y2(x_var = value, font_family = 'sans')
  expect_snapshot(error = TRUE, chart)
})

### Working stuff
test_that('bar_spacing works', {
chart <- dplyr::starwars |>
  dplyr::filter(hair_color %in% c('black', 'brown', 'white')) |>
  dplyr::group_by(gender) |>
  y2clerk::freq(hair_color, nas_group = FALSE) |>
  gg_grouped_y2(
    font_family = 'sans',
    fills = c('blue', 'red'),
    label_var = result,
    bar_spacing = 0.6
  )
    vdiffr::expect_doppelganger('bar_spacing works', chart)
})