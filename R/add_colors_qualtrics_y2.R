# Public function ---------------------------------------------------------
### add_colors_qualtrics_y2

#' Add base Qualtrics colors
#'
#' 4 Qualtrics colors: PURPLE, GREEN, BLUE, and TEAL
#'
#' Adds to your environment pre-set hex codes for the 4 base colors typically used
#' in Qualtrics projects. Each of the 4 colors has 5 shades.
#' Also adds grays, black, and white.
#'
#' @keywords color Qualtrics graph
#' @param palette DEFAULT = 2. Defaults to the newest Qualtrics color palette (2). Switch between the latest color palette and older color palettes, with 1 being the oldest.
#' @param case DEFAULT = 'upper'. Arg available only for palette 1. Will set color vectors to uppercase (e.g. BLUE_DARK) Can switch to 'lower' to revert to lowercase (e.g. blue_dark; older package versions were lowercase)
#' @param show_colors DEFAULT = TRUE Show the color palettes loaded into your environment
#' @export
#' @examples
#' add_colors_qualtrics_y2()
#' add_colors_qualtrics_y2('lower')

add_colors_qualtrics_y2 <- function(
  palette = 2,
  case = c('upper', 'lower'),
  show_colors = TRUE
) {
  case <- rlang::arg_match(case)

  if (palette == 2) {
    add_new_colors(
      show_colors = show_colors
    )
  } else if (case == 'upper' & palette == 1) {
      add_upper(
        show_colors = show_colors
      )
    } else if (case == 'lower' & palette == 1) {
      add_lower(
        show_colors = show_colors
      )
    }
}


# Private functions -------------------------------------------------------
### add_new_colors
add_new_colors <- function(
  show_colors = show_colors
) {

    add_color_palette_y2(
      c('#5F19E4', '#20DBA9', '#0767DC', '#03B3F0', '#B7B7B7'),
      c('Purple','Green','Blue','Sky_Blue','Gray'),
      c(1,1,1,1,1),
      show_colors = show_colors
    )

  qualtrics_colors <-
    list(
      WHITE = '#FFFFFF',
      BLACK = '#000000'
    )

  invisible(list2env(qualtrics_colors, envir = .GlobalEnv))

}




### add_upper
add_upper <- function(
  show_colors = show_colors
) {
  qualtrics_colors <-  list(
    PURPLE_DARKEST = '#090A10',
    PURPLE_DARK = '#2C314F',
    PURPLE = '#474E7E',
    PURPLE_MID = '#474E7E',
    PURPLE_LIGHT = '#8C90AE',
    PURPLE_LIGHTEST = '#D1D3DF',

    GREEN_DARKEST = '#395100',
    GREEN_DARK = '#5E8600',
    GREEN = '#97D700',
    GREEN_MID = '#97D700',
    GREEN_LIGHT = '#BEE660',
    GREEN_LIGHTEST = '#E5F5BF',

    BLUE_DARKEST = '#0A3D69',
    BLUE_DARK = '#0F5C9D',
    BLUE = '#147BD1',
    BLUE_MID = '#147BD1',
    BLUE_LIGHT = '#6CADE2',
    BLUE_LIGHTEST = '#C4DEF4',

    TEAL_DARKEST = '#256A6D',
    TEAL_DARK = '#389FA3',
    TEAL = '#4AD4D9',
    TEAL_MID = '#4AD4D9',
    TEAL_LIGHT = '#8EE4E7',
    TEAL_LIGHTEST = '#D2F4F6',

    GRAY_DARKEST = '#3D3D3D',
    GRAY_DARK = '#7A7A7A',
    GRAY = '#7A7A7A',
    GRAY_MID = '#B7B7B7',
    GRAY_LIGHT = '#D6D6D6',
    GRAY_LIGHTEST = '#F4F4F4',

    WHITE = '#FFFFFF',
    BLACK = '#000000'
  )

  if (show_colors == TRUE) {

    y2_colors_show <-
      qualtrics_colors %>%
      unlist() %>%
      as.vector()

    y2_colors_show[1:30] %>%
      scales::show_col(
        ncol = 6
      )
  }


  invisible(list2env(qualtrics_colors, envir = .GlobalEnv))
}

### add_lower
add_lower <- function(
  show_colors = show_colors
) {
  qualtrics_colors <-  list(
    purple_darkest = '#090A10',
    purple_dark = '#2C314F',
    purple = '#474E7E',
    purple_mid = '#474E7E',
    purple_light = '#8C90AE',
    purple_lightest = '#D1D3DF',

    green_darkest = '#395100',
    green_dark = '#5E8600',
    green = '#97D700',
    green_mid = '#97D700',
    green_light = '#BEE660',
    green_lightest = '#E5F5BF',

    blue_darkest = '#0A3D69',
    blue_dark = '#0F5C9D',
    blue = '#147BD1',
    blue_mid = '#147BD1',
    blue_light = '#6CADE2',
    blue_lightest = '#C4DEF4',

    teal_darkest = '#256A6D',
    teal_dark = '#389FA3',
    teal = '#4AD4D9',
    teal_mid = '#4AD4D9',
    teal_light = '#8EE4E7',
    teal_lightest = '#D2F4F6',

    gray_darkest = '#3D3D3D',
    gray_dark = '#7A7A7A',
    gray = '#7A7A7A',
    gray_mid = '#B7B7B7',
    gray_light = '#D6D6D6',
    gray_lightest = '#F4F4F4',

    white = '#FFFFFF',
    black = '#000000'
  )

  if (show_colors == TRUE) {

    y2_colors_show <-
      qualtrics_colors %>%
      unlist() %>%
      as.vector()

    y2_colors_show[1:30] %>%
      scales::show_col(
        ncol = 6
      )
  }

  invisible(list2env(qualtrics_colors, envir = .GlobalEnv))
}


