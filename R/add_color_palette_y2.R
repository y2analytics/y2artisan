# Public function ---------------------------------------------------------
### add_color_palette_y2

#' Add a 5 color palette
#'
#' Give a list of color names (e.g. blue) and the associated hex codes (e.g. #41536F). For each color given, add_color_palette_y2 will add a 5 color, monochromatic scale. Using position, choose where in the scale each hex code will start.
#'
#' @keywords color Qualtrics graph
#' @param hex_codes DEFAULT = '#41536F'; (Y2 blue) The hex code(s) of the color(s) for which you want to get a palette
#' @param color_names DEFAULT = 'Y2_BLUE'; The color label you want associated with each hex code. String can be in any case
#' @param position DEFAULT = 3; Position can be any of c(1, 2, 3, 4, 5). position = 1 will make the provided hex code the "DARKER" value while position = 5 will make it the "LIGHTER" value in the 5 point scale
#' @param colors_to_env DEFAULT = TRUE; Whether you want your palettes saved to your R environment
#' @param show_colors DEFAULT = TRUE; Whether you want a plot showing you the newly created color palettes for a quick reference
#' @export
#' @examples
#' add_color_palette_y2()
#'
#' add_color_palette_y2(
#'   hex_codes = c('#fd8420', '#558575', '#41536F'),
#'   color_names = c('Orange', 'gray', 'BLUE'),
#'   position = c(1, 3, 5),
#'   colors_to_env = TRUE,
#'   show_colors = TRUE
#' )


add_color_palette_y2 <-
  function(
    hex_codes = '#41536F',
    color_names = 'Y2_BLUE',
    position = 3,
    colors_to_env = TRUE,
    show_colors = TRUE
  ) {

    hex_codes <- stringr::str_to_upper(hex_codes)
    color_names <- stringr::str_to_upper(color_names)

    if (length(position) == 1){
      if (position == 3) {
        position <- rep(3, length(color_names))
      }
    }

    # Error to make sure matching # of hex codes and color names
    if (
      length(hex_codes) != length(color_names) |
      length(unique(color_names)) != length(color_names)
    ) {
      stop('Check to make sure there are names for every hex code and that all color_names are unique')
    }

    # Warning for non-unique hexcodes
    if (
      length(unique(hex_codes)) != length(hex_codes)
    ) {
      warning('Non-unique hexcodes supplied')
    }

    # Error to make if a custom hex code is given without a custom color name
    if (
      length(hex_codes) == 1
    ) {
      if (
        hex_codes != '#41536F' & color_names == 'Y2_BLUE'
      ) {
        stop('Custom color name required if custom hex code provided')
      }
    }

    # Error to make if a custom color name is given without a custom hex code
    if (
      length(hex_codes) == 1
    ) {
      if (
        hex_codes == '#41536F' & color_names != 'Y2_BLUE'
      ) {
        stop('Custom hex code required if custom color name provided')
      }
    }


    # Invalid position error
    if (
        !is.numeric(position)
    ) {
      stop('Position must be numeric or a vector of numeric elements')
    } else if (
      !all(
        position %% 1 == 0,
        all(position <=5),
        all(position >=1)
      )
    ){
      stop('Position elements must be 1, 2, 3, 4, or 5')
    }

    x <- seq(0, 1, length.out = 4)
    color_set_full <- NULL
    color_set_names_full <- NULL


    # Create palettes and names for all colors provided
    for (i in 1:length(hex_codes)) {

      if(position[i] == 1){

        color_set <-c(
          rev(scales::seq_gradient_pal("white", hex_codes[1])(seq(0, 1, length.out = 6)))
        ) %>%
          unique()

        color_set <- color_set[1:5]

        color_name_upper <-  stringr::str_to_upper(color_names[i])

        names(color_set) <-
          stringr::str_c(
            color_name_upper,
            c('_DARKER', '_DARK', '', '_LIGHT', '_LIGHTER')
          )

        color_set_full <- c(color_set_full, color_set)

      } else if (position[i] == 2) {

        color_set <-c(
          scales::seq_gradient_pal("black", hex_codes[i])(seq(0, 1, length.out = 3)),
          rev(scales::seq_gradient_pal("white", hex_codes[i])(seq(0, 1, length.out = 5)))
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

      } else if (position[i] == 3) {

        color_set <-c(
          scales::seq_gradient_pal("black", hex_codes[i])(seq(0, 1, length.out = 4)),
          rev(scales::seq_gradient_pal("white", hex_codes[i])(seq(0, 1, length.out = 4)))
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

      } else if (position[i] == 4) {

        color_set <-c(
          scales::seq_gradient_pal("black", hex_codes[i])(seq(0, 1, length.out = 5)),
          rev(scales::seq_gradient_pal("white", hex_codes[i])(seq(0, 1, length.out = 3)))
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

      } else {

        color_set <-c(
          scales::seq_gradient_pal("black", hex_codes[i])(seq(0, 1, length.out = 6))
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
