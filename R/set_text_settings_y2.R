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
#' @param text_column DEFAULT = label; Column from which labels are pulled
#' @param single DEFAULT = FALSE; Set to TRUE for use with a single bar chart
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
  font_family = 'BentonSans',
  text_column = label,
  single = FALSE
){

  label <- NULL

  set_text <-
    purrr::partial(
      officer::fp_text,
      font.size = font_size,
      color = font_color,
      font.family = font_family
    )

  variable_quoed <- dplyr::enquo(text_column)
  variable_char <- dplyr::quo_name(variable_quoed)

  label_char <- stringr::str_c('^', variable_char, '$')


  if( !any(dataset %>% names() %>% stringr::str_detect(label_char)) ){

    stop_message <- stringr::str_c('Missing column "', variable_char, '" in dataset')

    stop(stop_message)

  }

  if(single == FALSE){

    labels <-
      dataset %>%
      dplyr::distinct(
        {{ text_column }},
        .keep_all = TRUE
      ) %>%
      dplyr::pull({{ text_column }}) %>%
      as.character()

    text_settings <-
      replicate(
        length(labels),
        set_text(),
        simplify = FALSE
      )

    names(text_settings) <-
      labels

  } else {

    text_settings <-
      list(
        "result" = set_text()
      )

  }

  text_settings

}
