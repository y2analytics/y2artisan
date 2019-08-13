#### wordcloud_y2 ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller and lighter in color
#' @param dataset no default. Usually piped in from your main dataset
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = 'bluepurple'; 4 qualtrics colors as pre-made options: "bluepurple", "lime", "teal", "brightblue". May also specify a vector of 3 scaled colors ranging from lightest to darkest
#' @keywords openend open end wordcloud word cloud
#' @export
#' @examples
#' chart <- wordcloud_y2(QOPEN_END)
#' chart <- wordcloud_y2(QOPEN_END, lime)
#' chart <- wordcloud_y2(QOPEN_END, c('#FAEFF2', '#CC6078', '#6D0018'))

wordcloud_y2 <- function(
  dataset,
  variable,
  colors = 'bluepurple',
  top_x = 50,
  min_size = 1,
  max_size = 12
) {
  flag <- dplyr::enquo(variable)
  lows = dplyr::case_when(
    colors == 'bluepurple' ~ '#EEEEF3',
    colors == 'lime' ~ '#F6FBEB',
    colors == 'teal' ~ '#F1FCFC',
    colors == 'brightblue' ~ '#EDF4FB',
    T ~ colors[1]
  )
  mids = dplyr::case_when(
    colors == 'bluepurple' ~ '#BABDCF',
    colors == 'lime' ~ '#C6E881',
    colors == 'teal' ~ '#A5EAEC',
    colors == 'brightblue' ~ '#89B7E5',
    T ~ colors[2]
  )
  highs = dplyr::case_when(
    colors == 'bluepurple' ~ '#464E7E',
    colors == 'lime' ~ '#6A9D02',
    colors == 'teal' ~ '#389FA3',
    colors == 'brightblue' ~ '#0E5498',
    T ~ colors[3]
  )

  wordcloud_y2 <- openend_y2(dataset, !!flag, top_x)
  ggplot2::ggplot(
    wordcloud_y2,
    ggplot2::aes(
      x = 1,
      y = 1,
      size = n
    )
  ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        label = label,
        color = n
      ),
      segment.size = 0
    ) +
    ggplot2::scale_color_gradient2(low = lows, mid = mids, high = highs, midpoint = 0.5, guide = F) +
    ggplot2::scale_size(range = c(min_size, max_size), guide = FALSE) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

