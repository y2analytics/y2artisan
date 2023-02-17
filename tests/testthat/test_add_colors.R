
# add_color_palette_y2 ----------------------------------------------------

test_that('add_color_palette_y2, colors_to_env', {
  add_color_palette_y2(colors_to_env = FALSE)

  expect_error(exists(Y2_BLUE), "object 'Y2_BLUE' not found")
})


test_that('add_color_palette_y2, color_names to upper case', {
  add_color_palette_y2(
    hex_codes = '#123456',
    color_names = 'lower_case'
    )

  expect_error(exists(lower_case), "object 'lower_case' not found")
  expect_equal(LOWER_CASE, '#123456')
})


test_that('add_color_palette_y2, error messages', {

  expect_error(
    add_color_palette_y2(
      hex_codes = '#123456',
      color_names = c('color1', 'color2')
    ),
    'Check to make sure there are names for every hex code and that all color_names are unique'
  )

  # Ok, need to look into this error not erroring
  expect_warning(
    add_color_palette_y2(
      hex_codes = c('#123456', '#123456'),
      color_names = c('color1', 'color2')
    ),
    'Non-unique hexcodes supplied'
  )

  expect_error(
    add_color_palette_y2(
      hex_codes = '#123456',
      color_names = 'Y2_BLUE'
    ),
    'Custom color name required if custom hex code provided'
  )

  expect_error(
    add_color_palette_y2(
      hex_codes = '#41536F',
      color_names = 'color1'
    ),
    'Custom hex code required if custom color name provided'
  )

  expect_error(
    add_color_palette_y2(
      position = '1'
    ),
    'Position must be numeric or a vector of numeric elements'
  )

  expect_error(
    add_color_palette_y2(
      position = 0
    ),
    'Position elements must be 1, 2, 3, 4, or 5'
  )
})


test_that('add_colors_facebook_y2, position 5', {
  add_color_palette_y2(
    position = 5
  )
  end_darkest <- scales::seq_gradient_pal("black", '#41536F')(seq(0, 1, length.out = 6))[2]
  end_lightest <- rev(scales::seq_gradient_pal("white", '#41536F')(seq(0, 1, length.out = 2)))[1]

  expect_equal(Y2_BLUE_LIGHTER, '#41536F') # position 5
  expect_equal(Y2_BLUE_LIGHTER, end_lightest)
  expect_equal(Y2_BLUE_DARKER, end_darkest)
})


test_that('add_colors_facebook_y2, position 4', {
  add_color_palette_y2(
    position = 4
  )
  end_darkest <- scales::seq_gradient_pal("black", '#41536F')(seq(0, 1, length.out = 5))[2]
  end_lightest <- rev(scales::seq_gradient_pal("white", '#41536F')(seq(0, 1, length.out = 3)))[2]

  expect_equal(Y2_BLUE_LIGHT, '#41536F') # position 4
  expect_equal(Y2_BLUE_LIGHTER, end_lightest)
  expect_equal(Y2_BLUE_DARKER, end_darkest)
})


test_that('add_colors_facebook_y2, position 3', {
  add_color_palette_y2(
    position = 3
  )
  end_darkest <- scales::seq_gradient_pal("black", '#41536F')(seq(0, 1, length.out = 4))[2]
  end_lightest <- rev(scales::seq_gradient_pal("white", '#41536F')(seq(0, 1, length.out = 4)))[3]

  expect_equal(Y2_BLUE, '#41536F') # position 3
  expect_equal(Y2_BLUE_LIGHTER, end_lightest)
  expect_equal(Y2_BLUE_DARKER, end_darkest)
})


test_that('add_colors_facebook_y2, position 2', {
  add_color_palette_y2(
    position = 2
  )
  end_darkest <- scales::seq_gradient_pal("black", '#41536F')(seq(0, 1, length.out = 3))[2]
  end_lightest <- rev(scales::seq_gradient_pal("white", '#41536F')(seq(0, 1, length.out = 5)))[4]

  expect_equal(Y2_BLUE_DARK, '#41536F') # position 2
  expect_equal(Y2_BLUE_LIGHTER, end_lightest)
  expect_equal(Y2_BLUE_DARKER, end_darkest)
})


test_that('add_colors_facebook_y2, position 1', {
  add_color_palette_y2(
    position = 1
  )
  end_darkest <- scales::seq_gradient_pal("black", '#41536F')(seq(0, 1, length.out = 2))[2]
  end_lightest <- rev(scales::seq_gradient_pal("white", '#41536F')(seq(0, 1, length.out = 6)))[5]

  expect_equal(Y2_BLUE_DARKER, '#41536F') # position 1
  expect_equal(Y2_BLUE_LIGHTER, end_lightest)
  expect_equal(Y2_BLUE_DARKER, end_darkest)
})





# add_colors_facebook_y2 --------------------------------------------------

test_that('add_colors_facebook_y2, colors to env', {
  add_colors_facebook_y2()

  expect_equal(BLUE_MARKETING, '#3B5998')
  expect_equal(ALUMINUM, '#A3CEDF')
})





# add_colors_internal_y2 --------------------------------------------------

test_that('add_colors_internal_y2, colors to env', {
  add_colors_internal_y2()

  expect_equal(BLUE, '#1A497A')
  expect_equal(ORANGE, '#FFB022')
})

test_that('add_colors_internal_y2, compliments', {
  add_colors_internal_y2(compliments = TRUE)

  expect_equal(PINK, '#F0A8C5')
})



# add_colors_microsoft_y2 -------------------------------------------------

test_that('add_colors_microsoft_y2, colors to env', {
  add_colors_microsoft_y2()

  expect_equal(BLUE_LIGHT, '#62E6FF')
  expect_equal(TEAL_DARK, '#254B47')
})





# add_colors_qualtrics_y2 -------------------------------------------------

test_that('add_colors_qualtrics_y2, palette 1 (oldest), upper', {
  add_colors_qualtrics_y2(
    palette = 1,
    case = 'upper'
    )

  expect_equal(PURPLE_DARK, '#2C314F')
  expect_equal(TEAL_LIGHTEST, '#D2F4F6')
})


test_that('add_colors_qualtrics_y2, palette 1 (oldest), lower', {
  add_colors_qualtrics_y2(
    palette = 1,
    case = 'lower'
  )

  expect_equal(purple_dark, '#2C314F')
  expect_equal(teal_lightest, '#D2F4F6')
})


test_that('add_colors_qualtrics_y2, palette 2 (newest)', {
  add_colors_qualtrics_y2(palette = 2)

  expect_equal(PURPLE_DARK, '#8B4FEB')
  expect_equal(GREEN_LIGHT, '#BBF2DC')
})

