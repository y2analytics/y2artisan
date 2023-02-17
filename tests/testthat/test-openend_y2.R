
test_that("openend_y2 - formatting", {
  responses <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    ),
    var2 = rep('test test test', 6)
  )

  test <- responses %>% openend_y2(var1)
  test_names <- responses %>% openend_y2(var1) %>% names()
  expect_equal(class(test)[1], 'tbl_df')
  expect_equal(test_names, c('variable', 'value', 'label', 'n', 'stat', 'result'))
})


test_that("openend_y2 - n and result correctly calculated", {
  responses <- tibble::tibble(
    var1 = c(
      'I like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight",
      'One last sentence about dogs',
      'Cats collars are typically cooler than dogs'
    )
  )

  test <- responses %>% openend_y2(var1)
  expect_equal(test$n[1], 6)
  expect_equal(test$result[1], 1.00)
})


test_that("openend_y2 - remove filler words and special characters", {
  responses <- tibble::tibble(
    var1 = c(
      'I do not like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight and whatnot",
      'One last sentence about dogs.com',
      'Cats collars are typically cooler than dogs'
    )
  )

  test <- responses %>% openend_y2(var1)
  remove_apostrophe <- stringr::str_detect(test$label, "DOG'S") %>% sum()
  do_not <- test %>% dplyr::filter(label == 'DO') %>% nrow()
  dont <- test %>% dplyr::filter(label == 'DONT') %>% nrow()
  and <- test %>% dplyr::filter(label == 'AND') %>% nrow()
  dot_com <- stringr::str_detect(test$label, "COM") %>% sum()
  expect_equal(remove_apostrophe, 0)
  expect_equal(do_not, 0)
  expect_equal(dont, 0)
  expect_equal(and, 0)
  expect_equal(dot_com, 0)
})


test_that("openend_y2 - top_x", {
  responses <- tibble::tibble(
    var1 = c(
      'I do not like to talk about dogs',
      'Dogs are cool but cats are aight too',
      'I prefer dogs over cats',
      "My dog's collars are always too tight and whatnot",
      'One last sentence about dogs.com',
      'Cats collars are typically cooler than dogs'
    )
  )

  test_2 <- responses %>% openend_y2(var1, top_x = 2) %>% nrow()
  test_10 <- responses %>% openend_y2(var1, top_x = 10) %>% nrow()
  expect_equal(test_2, 2)
  expect_equal(test_10, 10)
})


test_that("openend_y2 - missing rows and repeat words", {
  responses <- tibble::tibble(
    var1 = c(
      'I do not like to talk about dogs',
      '',
      'I prefer dogs over cats. test test test test test test test test test',
      "My dog's collars are always too tight and whatnot",
      NA_character_,
      'Cats collars are typically cooler than dogs'
    )
  )

  test <- responses %>% openend_y2(var1)
  count_test <- test %>% dplyr::filter(label == 'TEST') %>% dplyr::pull(n)
  count_dog <- test %>% dplyr::filter(label == 'DOGS') %>% dplyr::pull(n)
  expect_equal(count_test, 1)
  expect_equal(count_dog, 4)
})

