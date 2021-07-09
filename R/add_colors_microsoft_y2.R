# Public function ---------------------------------------------------------
### add_colors_microsoft_y2

#' Add base Microsoft colors
#'
#'  Microsoft colors: ORANGE, YELLOW, GREEN, TEAL, BLUE, PURPLE, GRAY
#'
#' Adds to your environment pre-set hex codes for the 7 base colors used
#' in Microsoft themed projects. Each of the 7 colors has 3 shades except Gray, which has 6 shades.
#' Also adds black and white.
#'
#' @keywords color microsoft graph
#' @param palette DEFAULT = 1. Defaults to the newest microsoft color palette. Switch between the latest color palette and older color palettes.
#' @param show_colors DEFAULT = TRUE Show the color palettes loaded into your environment
#' @export
#' @examples
#' add_colors_microsoft_y2()
#' add_colors_microsoft_y2(show_colors = TRUE)

add_colors_microsoft_y2 <- function(
  palette = c('RE&S')[1],
  show_colors = TRUE
) {

  if (palette == 'RE&S') {
    set_res_palettes(
      show_colors = show_colors
    )
  }
}


# Private functions -------------------------------------------------------
### add_new_colors
set_res_palettes <- function(
  show_colors
){
  msoft_colors <-  list(

    BLUE_DARK = '#243A5E',
    BLUE = '#2C78D4',
    BLUE_LIGHT = '#62E6FF',

    ORANGE_DARK = '#6C2A29',
    ORANGE = '#D73D27',
    ORANGE_LIGHT = '#F28C32',

    YELLOW_DARK = '#6B4B1C',
    YELLOW = '#F6B714',
    YELLOW_LIGHT = '#F7EC17',

    PURPLE_DARK = '#392D57',
    PURPLE = '#7E61AA',
    PURPLE_LIGHT = '#BF9FCB',

    GREEN_DARK = '#1C4A24',
    GREEN = '#337B3E',
    GREEN_LIGHT = '#A2CD3A',

    TEAL_DARK = '#254B47',
    TEAL = '#378775',
    TEAL_LIGHT = '#60C6BF',

    GRAY_DARKEST = '#2E2E2E',
    GRAY_DARKER = '#4F5151',
    GRAY_DARK = '#747474',
    GRAY_LIGHT = '#D2D2D3',
    GRAY_LIGHTER = '#E6E7E7',
    GRAY_LIGHTEST = '#F2F3F3',

    WHITE = '#FFFFFF',
    BLACK = '#000000'
  )


  if (show_colors == TRUE) {

    msoft_colors_show <-
      msoft_colors %>%
      unlist() %>%
      as.vector()

    msoft_colors_show[-c(25:26)] %>%
      scales::show_col(
        cex_label = .66,
        ncol = 3
      )
  }

  invisible(list2env(msoft_colors, envir = .GlobalEnv))
}
