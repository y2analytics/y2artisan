# Public function ---------------------------------------------------------
### add_colors_y2

#' Add base Qulatrics colors
#'
#' 4 Qualtrics colors: PURPLE, GREEN, BLUE, and TEAL
#'
#' Adds to your environment pre-set hex codes for the 4 base colors typically used
#' in Qualtrics projects. Each of the 4 colors has 5 shades.
#' Also adds grays, black, and white.
#'
#' @keywords color Qualtrics graph
#' @param case DEFAULT = 'upper'. Will set color vectors to uppercase (e.g. BLUE_DARK) Can switch to 'lower' to revert to lowercase (e.g. blue_dark; older package versions were lowercase)
#' @export
#' @examples
#' add_colors_y2()
#' add_colors_y2('lower')

add_colors_y2 <- function(
  case = c('upper', 'lower')
  ) {
  case <- rlang::arg_match(case)

  if (case[1] == 'upper') {
    add_upper()
  } else {
    add_lower()
  }
}



# Private functions -------------------------------------------------------
### add_upper
add_upper <- function(){
  qualtrics_colors <-  list(
    PURPLE = '#474E7E',
    PURPLE_LIGHTEST = '#D1D3DF',
    PURPLE_LIGHT = '#8C90AE',
    PURPLE_MID = '#474E7E',
    PURPLE_DARK = '#2C314F',
    PURPLE_DARKEST = '#090A10',

    GREEN = '#97D700',
    GREEN_LIGHTEST = '#E5F5BF',
    GREEN_LIGHT = '#BEE660',
    GREEN_MID = '#97D700',
    GREEN_DARK = '#5E8600',
    GREEN_DARKEST = '#395100',

    BLUE = '#147BD1',
    BLUE_LIGHTEST = '#C4DEF4',
    BLUE_LIGHT = '#6CADE2',
    BLUE_MID = '#147BD1',
    BLUE_DARK = '#0F5C9D',
    BLUE_DARKEST = '#0A3D69',

    TEAL = '#4AD4D9',
    TEAL_LIGHTEST = '#D2F4F6',
    TEAL_LIGHT = '#8EE4E7',
    TEAL_MID = '#4AD4D9',
    TEAL_DARK = '#389FA3',
    TEAL_DARKEST = '#256A6D',

    GRAY = '#7A7A7A',
    GRAY_LIGHTEST = '#F4F4F4',
    GRAY_LIGHT = '#D6D6D6',
    GRAY_MID = '#B7B7B7',
    GRAY_DARK = '#7A7A7A',
    GRAY_DARKEST = '#3D3D3D',

    WHITE = '#FFFFFF',
    BLACK = '#000000'
  )
  invisible(list2env(qualtrics_colors, envir = .GlobalEnv))
}

### add_lower
add_lower <- function(){
  qualtrics_colors <-  list(
    purple = '#474E7E',
    purple_lightest = '#D1D3DF',
    purple_light = '#8C90AE',
    purple_mid = '#474E7E',
    purple_dark = '#2C314F',
    purple_darkest = '#090A10',

    green = '#97D700',
    green_lightest = '#E5F5BF',
    green_light = '#BEE660',
    green_mid = '#97D700',
    green_dark = '#5E8600',
    green_darkest = '#395100',

    blue = '#147BD1',
    blue_lightest = '#C4DEF4',
    blue_light = '#6CADE2',
    blue_mid = '#147BD1',
    blue_dark = '#0F5C9D',
    blue_darkest = '#0A3D69',

    teal = '#4AD4D9',
    teal_lightest = '#D2F4F6',
    teal_light = '#8EE4E7',
    teal_mid = '#4AD4D9',
    teal_dark = '#389FA3',
    teal_darkest = '#256A6D',

    gray = '#7A7A7A',
    gray_lightest = '#F4F4F4',
    gray_light = '#D6D6D6',
    gray_mid = '#B7B7B7',
    gray_dark = '#7A7A7A',
    gray_darkest = '#3D3D3D',

    white = '#FFFFFF',
    black = '#000000'
  )
  invisible(list2env(qualtrics_colors, envir = .GlobalEnv))
}

