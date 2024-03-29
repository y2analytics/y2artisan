#### wordcloud_mono_y2 ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller
#' @param dataset no default. Usually piped in from your main dataset
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = '#474E7E' (bluepurple from Qualtrics template). All words are the same color. Any color may be specified as a hexcode
#' @param max_size DEFAULT = 12; the largest text size for the word with the highest frequency#' @keywords openend open end wordcloud word cloud
#' @param min_size DEFAULT = 1; the smallest text size for the word with the lowest frequency
#' @param top_x DEFAULT = 50; Shows the top X most commonly mentioned words you want to see from the open-end
#' @param font_family DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @export
#' @examples
#' responses <- tibble::tibble(
#'   var1 = c(
#'     'I like to talk about dogs',
#'     'Dogs are cool but cats are aight too',
#'     'I prefer dogs over cats',
#'     "My dog's collars are always too tight",
#'     'One last sentence about dogs',
#'     'Cats collars are typically cooler than dogs'
#'   )
#' )
#'
#' responses %>% wordcloud_mono_y2(var1, font_family = "Arial")
#' responses %>% wordcloud_mono_y2(var1, 'red', font_family = "Arial")

wordcloud_mono_y2 <- function(
  dataset,
  variable,
  colors = '#474E7E',
  max_size = 12,
  min_size = 1,
  font_family = 'Flama',
  top_x = 50
) {

  if (
    (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
  ) {
    stop("The font you specified in the 'font_family' argument does not exist in your R session")
  }

  flag_var <- dplyr::enquo(variable)
  frequencies <- openend_y2(dataset, !!flag_var, top_x)
  chart <- ggplot2::ggplot(
    frequencies,
    ggplot2::aes(
      x = 1,
      y = 1,
      size = .data$n
    )
  ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        label = .data$label,
        color = variable
      ),
      segment.size = 0,
      family = font_family
    ) +
    ggplot2::scale_color_manual(values = colors, guide = 'none') +
    ggplot2::scale_size(range = c(min_size, max_size), guide = 'none') +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
  ggplot2::theme_minimal()+
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    text = ggplot2::element_text(family = font_family)
  )
  return(chart)
}
