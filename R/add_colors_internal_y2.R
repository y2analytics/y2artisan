# Public function ---------------------------------------------------------
### add_colors_internal_y2

#' Add base Y2 colors
#'
#' 3 Y2 colors: BLUE, GRAY and ORANGE
#'
#' Adds to your environment pre-set hex codes for the 3 base colors used
#' in Y2 themed projects. Each of the 3 colors has 5 shades.
#' Also adds black and white.
#'
#' @keywords color y2 graph
#' @param palette DEFAULT = 1. Defaults to the newest Y2 color palette. Switch between the latest color palette and older color palettes.
#' @param compliments DEFAULT = FALSE. Add complimentary colors chosen by the Y2 team (Red, Pink, Yellow, Green, Turquoise)
#' @param show_colors DEFAULT = TRUE Show the color palettes loaded into your environment
#' @export
#' @examples
#' add_colors_internal_y2()
#' add_colors_internal_y2(TRUE)

add_colors_internal_y2 <- function(
  palette = 1,
  compliments = FALSE,
  show_colors = TRUE
) {

  if (palette == 1) {
    set_y2_palettes(
      compliments = compliments,
      show_colors = show_colors
    )
  }
}


# Private functions -------------------------------------------------------
### add_new_colors
set_y2_palettes <- function(
  compliments = compliments,
  show_colors
){
  y2_colors <-  list(

    BLUE_DARKER = '#041B32',
    BLUE_DARK = '#082C51',
    BLUE = '#1A497A',
    BLUE_LIGHT = '#3E85BD',
    BLUE_LIGHTER = '#9EBCDB',

    ORANGE_DARKER = '#CC6F36',
    ORANGE_DARK = '#E8903F',
    ORANGE = '#FFB022',
    ORANGE_LIGHT = '#F3C974',
    ORANGE_LIGHTER = '#FFEBC2',

    GRAY_DARKER = '#222222',
    GRAY_DARK = '#333333',
    GRAY = '#666666',
    GRAY_LIGHT = '#B3B3B3',
    GRAY_LIGHTER = '#D9D9D9',

    WHITE = '#FFFFFF',
    BLACK = '#000000'
  )

  if (compliments == TRUE) {
    y2_colors <-
      c(
        y2_colors,
        TURQUOISE = '#77E0CA',
        YELLOW = '#FDED07',
        GREEN = '#5ABE46',
        PINK = '#F0A8C5',
        RED = '#AD0001'
      )
  }

  if (show_colors == TRUE) {

    y2_colors_show <-
      y2_colors %>%
      unlist() %>%
      as.vector()

    y2_colors_show[-c(16:17)] %>%
      scales::show_col(
        ncol = 5
        )
  }

  invisible(list2env(y2_colors, envir = .GlobalEnv))
}


