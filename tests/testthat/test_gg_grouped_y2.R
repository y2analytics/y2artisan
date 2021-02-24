# Setup -------------------------------------------------------------------
library(dplyr)
library(y2clerk)
library(testthat)
library(y2artisan)


#### gg_grouped_y2 ####
context("gg_grouped_y2")


#### Errors when changing the defaults? ####
test_that("all defaults", {
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    result = rep(.2, 10),
    group_var = c(1,1,1,1,1,2,2,2,2,2),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_grouped_y2(font_family = "sans")
  expect_error(chart, NA)
})


test_that("x_var", {
  chart <- tibble::tibble(
    label = rep(c(1:5), 2) %>% as.character(),
    value = label %>% as.numeric,
    result = rep(.2, 10),
    group_var = c(1,1,1,1,1,2,2,2,2,2),
    percent_label = stringr::str_c(result * 100, '%')
  ) %>%
    gg_grouped_y2(x_var = value, font_family = "sans")
  expect_error(chart, NA)
})

