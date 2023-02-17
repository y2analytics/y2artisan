
#### Errors when changing the defaults? ####
test_that('all defaults', {
  FILLS <- c('#1D1401', '#765003', '#EC9F07', '#F5CF83', '#FDF3E0')
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    result = rep(.2, 10),
    group_var = c(rep('One', 5), rep('Two', 5)),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_stacked_y2(fills = FILLS, font_family = 'sans')
  chart
  expect_error(chart, NA)
})


test_that('x_var', {
  FILLS <- c('#1D1401', '#765003', '#EC9F07', '#F5CF83', '#FDF3E0')
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    value = label %>% as.numeric,
    result = rep(.2, 10),
    group_var = c(rep('One', 5), rep('Two', 5)),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
  gg_stacked_y2(
    fills = FILLS,
    direction = 'vertical',
    font_family = 'sans'
    )
  chart
expect_error(chart, NA)
})
