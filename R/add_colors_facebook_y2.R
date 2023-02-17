# Public function ---------------------------------------------------------
### add_colors_facebook_y2

#' Add base Facebook colors
#'
#' Primary/Secondary Facebook colors: BLUE_MARKETING, BLUE_FIG, BLUE_HIGHLIGHT, TEAL, LIME, LEMON, ORANGE
#'
#' Adds to your environment pre-set hex codes for the Facebook blues along with
#' 4 secondary colors used in Facebook themed projects. There are an additional
#' 11 tertiary colors. The 4 blues have 7 shades. Also adds BLACK, WHITE, and
#' GRAY, while optionally displaying the colors/hex codes for you to see.
#'
#' @keywords color facebook graph
#' @param palette DEFAULT = 1. Defaults to the newest facebook color palette (1).
#' @param show_colors DEFAULT = TRUE Show the color palettes loaded into your environment
#' @export
#' @examples
#' add_colors_facebook_y2()
#' add_colors_facebook_y2(show_colors = TRUE)

add_colors_facebook_y2 <- function(
  palette = 1,
  show_colors = TRUE
) {

  if (palette == 1) {
    set_fb_palettes(
      show_colors = show_colors
    )
  }
}


# Private functions -------------------------------------------------------
### add_new_colors
set_fb_palettes <- function(
  show_colors
) {
  fb_colors <-  list(

    # PRIMARY COLORS
    BLUE_MARKETING = '#3B5998',
    BLUE_FIG = '#4A66AD',
    BLUE_HIGHLIGHT = '#5890FF',

    # SECONDARY COLORS
    TEAL = '#6BCEBB',
    LIME = '#A3CE71',
    LEMON = '#FCD872',
    ORANGE = '#F7923B',

    # PRIMARY BLUES WITH 7 SHADES
    BLUE_MARKETING_LIGHTEST = '#CED6E5',
    BLUE_MARKETING_LIGHTER = '#9DACCC',
    BLUE_MARKETING_LIGHT = '#6C83B2',
    BLUE_MARKETING = '#3B5998',
    BLUE_MARKETING_DARK = '#2C4372',
    BLUE_MARKETING_DARKER = '#1D2C4C',
    BLUE_MARKETING_DARKEST = '#070B13',

    BLUE_FIG_LIGHTEST = '#D2D9EA',
    BLUE_FIG_LIGHTER = '#A5B3D6',
    BLUE_FIG_LIGHT = '#778CC2',
    BLUE_FIG = '#4A66AD',
    BLUE_FIG_DARK = '#384D82',
    BLUE_FIG_DARKER = '#253356',
    BLUE_FIG_DARKEST = '#090D16',

    BLUE_HIGHLIGHT_LIGHTEST = '#D5E3FF',
    BLUE_HIGHLIGHT_LIGHTER = '#ACC8FF',
    BLUE_HIGHLIGHT_LIGHT = '#82ACFF',
    BLUE_HIGHLIGHT = '#5890FF',
    BLUE_HIGHLIGHT_DARK = '#426CBF',
    BLUE_HIGHLIGHT_DARKER = '#2C4880',
    BLUE_HIGHLIGHT_DARKEST = '#0B1220',

    BLUE_ROYAL_LIGHTEST = '#CEDCFA',
    BLUE_ROYAL_LIGHTER = '#9CBAF4',
    BLUE_ROYAL_LIGHT = '#6B98EF',
    BLUE_ROYAL = '#3975EA',
    BLUE_ROYAL_DARK = '#2B58AF',
    BLUE_ROYAL_DARKER = '#1C3A75',
    BLUE_ROYAL_DARKEST = '#070F1D',


    # TERTIARY COLORS
    SEAFOAM = '#54C7EC',
    ALUMINUM = '#A3CEDF',
    SLATE = '#B9CAD2',
    PURPLE = '#4B4AA4',
    GRAPE = '#8C72CB',
    FUCHSIA = '#D855A9',
    PINK = '#EC7EBD',
    POPPY = '#F34F46',
    CHERRY = '#F35269',
    TOMATO = '#FB724B',
    GRASS = '#60CE82',


    WHITE = '#FFFFFF',
    BLACK = '#000000',
    GRAY = '#E9EAED'
  )


  if (show_colors == TRUE) {

    fb_colors_show <-
      fb_colors %>%
      unlist() %>%
      as.vector()

    fb_colors_show %>%
      scales::show_col(
        cex_label = .66,
        ncol = 7
      )
  }

  invisible(list2env(fb_colors, envir = .GlobalEnv))
}
