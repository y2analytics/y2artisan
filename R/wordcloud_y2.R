#### wordcloud_y2 ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller and lighter in color
#' @param dataset no default. Usually piped in from your main dataset
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = 'bluepurple'; 4 qualtrics colors as pre-made options: "bluepurple", "lime", "teal", "brightblue". May also specify a vector of 3 scaled colors ranging from lightest to darkest
#' @param max_size DEFAULT = 12; the largest text size for the word with the highest frequency
#' @param min_size DEFAULT = 1; the smallest text size for the word with the lowest frequency
#' @param top_x DEFAULT = 50; Shows the top X most commonly mentioned words you want to see from the open-end
#' @param font_family DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @keywords openend open end wordcloud word cloud
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
#' RED_DARK <- '#C61616'
#' RED_MID <- '#E38B8B'
#' RED_LIGHT <- '#F9E6E6'
#'
#' # If you want to use a specific color, you must specify three scalar shades of that color
#' responses %>% wordcloud_y2(var1, font_family = "Arial")
#' responses %>% wordcloud_y2(var1, c(RED_LIGHT, RED_MID, RED_DARK), font_family = "Arial",)

wordcloud_y2 <- function(
  dataset,
  variable,
  colors = 'bluepurple',
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
  lows = dplyr::case_when(
    colors == 'bluepurple' ~ '#EEEEF3',
    colors == 'lime' ~ '#F6FBEB',
    colors == 'teal' ~ '#F1FCFC',
    colors == 'brightblue' ~ '#EDF4FB',
    TRUE ~ colors[1]
  )
  mids = dplyr::case_when(
    colors == 'bluepurple' ~ '#BABDCF',
    colors == 'lime' ~ '#C6E881',
    colors == 'teal' ~ '#A5EAEC',
    colors == 'brightblue' ~ '#89B7E5',
    TRUE ~ colors[2]
  )
  highs = dplyr::case_when(
    colors == 'bluepurple' ~ '#464E7E',
    colors == 'lime' ~ '#6A9D02',
    colors == 'teal' ~ '#389FA3',
    colors == 'brightblue' ~ '#0E5498',
    TRUE ~ colors[3]
  )

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
        color = .data$n
      ),
      segment.size = 0,
      family = font_family
    ) +
    ggplot2::scale_color_gradient2(
      low = lows,
      mid = mids,
      high = highs,
      midpoint = 0.5,
      guide = 'none'
      ) +
    ggplot2::scale_size(
      range = c(min_size, max_size),
      guide = 'none'
      ) +
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

