# Public function ---------------------------------------------------------
### set_color_settings

#' Setup the color setting for ms charts
#'
#' Save a frequencies object using freqs(). Provide colors to the set_color_settings function. The colors will be automatically matched to the "label" column of the frequencies.
#'
#' @keywords color settings ms charts
#' @param ... Colors to use in your ms_chart
#' @param dataset DEFAULT = frequencies; A frequency table named frequencies
#' @export
#' @examples
#'
#' add_colors_internal_y2()
#'
#' frequencies <-
#'   mtcars %>%
#'   y2clerk::freqs(
#'     cyl
#'   )
#'
#' set_color_settings_y2(
#'   BLUE_DARKER,
#'   BLUE,
#'   BLUE_LIGHTER
#'  )

set_color_settings_y2 <- function(
    ...,
    dataset = frequencies
  ){

  colors <- list(...) %>% purrr::as_vector()


  if( !any(dataset %>% names() %>% stringr::str_detect('^label$')) ){

    stop('Missing column "label" in dataset')

  }

  labels <-
    dataset %>%
    dplyr::distinct(
      .data$label,
      .keep_all = TRUE
    ) %>%
    dplyr::pull(.data$label) %>%
    as.character()

  if( length(colors) < length(labels) ){

    stop(
      stringr::str_c(
        'Not enough colors provided. Please provide ',
        length(labels) - length(colors),
        ' more color(s) OR check the "label" column in the data.'
      )
    )

  } else if( length(colors) > length(labels) ){

    stop(

      stringr::str_c(
        'Too many colors provided. Please provide ',
        length(colors) - length(labels),
        ' less color(s) OR check the "label" column in the data.'
      )
    )

  }


  names(colors) <-
    labels

  colors

}
