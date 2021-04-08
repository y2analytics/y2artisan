# Public function ---------------------------------------------------------
### add_color_palette_y2

#' Add a 5 color palette
#'
#' Give a list of color names (e.g. blue) and the associated hex codes (e.g. #41536F). For each color given, add_color_palette_y2 will add a 5 color, monochromatic scale
#'
#' @keywords color Qualtrics graph
#' @param hex_codes DEFAULT = '#41536F'; (Y2 blue) The hex code(s) of the color(s) for which you want to get a palette
#' @param color_names DEFAULT = 'BLUE'; The color label you want associated with each hex code. String can be in any case
#' @param colors_to_env DEFAULT = TRUE; Whether you want your palettes saved to your R environment
#' @param show_colors DEFAULT = TRUE; Whether you want a plot showing you the newly created color palettes for a quick reference
#' @export
#' @examples
#' add_color_palette_y2()
#'
#' add_color_palette_y2(
#'   hex_codes = c('#fd8420', '#558575', '#41536F'),
#'   color_names = c('Orange', 'gray', 'BLUE'),
#'   colors_to_env = TRUE,
#'   show_colors = TRUE
#' )


add_color_palette_y2 <-
  function(
    hex_codes = '#41536F',
    color_names = 'BLUE',
    colors_to_env = TRUE,
    show_colors = TRUE
  ) {

    hex_codes <- stringr::str_to_upper(hex_codes)
    color_names <- stringr::str_to_upper(color_names)

    # Error to make sure matching # of hex codes and color names
    if (
      length(hex_codes) != length(color_names) |
      length(unique(hex_codes)) != length(hex_codes) |
      length(unique(color_names)) != length(color_names)
    ) {
      stop('Check to make sure there are names for every hex code and that all hex_codes and color_names are unique')
    }

    x <- seq(0, 1, length.out = 4)
    color_set_full <- NULL
    color_set_names_full <- NULL


    # Create palettes and names for all colors provided
    for (i in 1:length(hex_codes)) {
      color_set <-c(
        scales::seq_gradient_pal("black", hex_codes[i])(x),
        rev(scales::seq_gradient_pal("white", hex_codes[i])(x))
      ) %>%
        unique()

      color_set <- color_set[2:6]

      color_name_upper <-  stringr::str_to_upper(color_names[i])

      names(color_set) <-
        stringr::str_c(
          color_name_upper,
          c('_DARKER', '_DARK', '', '_LIGHT', '_LIGHTER')
        )

      color_set_full <- c(color_set_full, color_set)
    }


    # Save out palettes to environment
    if (colors_to_env == TRUE) {
      color_set_full %>%
        as.list() %>%
        list2env(envir = .GlobalEnv) %>%
        invisible()
    } else {
      print('Colors not added to environment')
    }


    # Display color grid/plot
    if (show_colors == TRUE) {
      color_set_full %>%
        unname() %>%
        scales::show_col(ncol = 5)
    }
  }
