# Public function ---------------------------------------------------------
### set_color_settings

#' Setup the color setting for ms charts
#'
#' Save a frequencies object using freqs(). Provide colors to the set_color_settings function. The colors will be automatically matched to the "label" column of the frequencies or you can manually choose a different column .
#'
#' @keywords color settings ms charts
#' @param ... Colors to use in your ms_chart
#' @param dataset DEFAULT = frequencies; A frequency table named frequencies
#' @param color_column DEFAULT = label; Column from which labels are pulled
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
#'
#' frequencies <-
#'   mtcars %>%
#'   dplyr::group_by(
#'     gear
#'   ) %>%
#'   y2clerk::freqs(
#'     cyl
#'   )
#'
#' set_color_settings_y2(
#'   BLUE_DARKER,
#'   BLUE,
#'   BLUE_LIGHTER,
#'   color_column = group_var
#'  )

set_color_settings_y2 <- function(
    ...,
  dataset = frequencies,
  color_column = label
  ){

  label <- NULL

  variable_quoed <- dplyr::enquo(color_column)
  variable_char <- dplyr::quo_name(variable_quoed)

  label_char <- stringr::str_c('^', variable_char, '$')


  if( !any(dataset %>% names() %>% stringr::str_detect(label_char)) ){

    stop('Missing column "', variable_char, '" in dataset')

  }

  colors <- list(...) %>% purrr::as_vector()

  labels <-
    dataset %>%
    dplyr::ungroup() %>%
    dplyr::distinct(
      {{ color_column }},
      .keep_all = TRUE
    ) %>%
    dplyr::pull({{ color_column }}) %>%
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
