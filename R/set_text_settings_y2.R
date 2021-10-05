# Public function ---------------------------------------------------------
### set_text_settings_y2

#' Setup the text setting for ms charts
#'
#' Save a frequencies object using freqs(). The text settings will be automatically matched to the "label" column of the frequencies.
#'
#' @keywords text settings ms charts
#' @param dataset DEFAULT = frequencies; A frequency table named frequencies
#' @param font_size DEFAULT = 14; Font size applied to all text in chart labels
#' @param font_color DEFAULT = 'white'; Color applied to all text in chart labels
#' @param font_family DEFAULT = 'BentonSans'; Font family applied to all text in chart labels
#' @export
#' @examples
#'
#' frequencies <-
#'   mtcars %>%
#'   y2clerk::freqs(
#'     cyl
#'   )
#'
#' set_text_settings_y2()
#'
#' set_text_settings_y2(
#'   font_size = 28,
#'   font_color = 'black'
#'  )


set_text_settings_y2 <- function(
  dataset = frequencies,
  font_size = 14,
  font_color = 'white',
  font_family = 'BentonSans'
){

  set_text <-
    purrr::partial(
      fp_text,
      font.size = font_size,
      color = font_color,
      font.family = font_family
    )

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

  text_settings <-
    replicate(
      length(labels),
      set_text(),
      simplify = FALSE
    )

  names(text_settings) <-
    labels

  text_settings

}
