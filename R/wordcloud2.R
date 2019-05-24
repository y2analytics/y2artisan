#### wordcloud2 ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller
#' @param dataset no default. Usually piped in from your main dataset
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = '#474E7E' (bluepurple from Qualtrics template). All words are the same color. Any color may be specified as a hexcode
#' @keywords openend open end wordcloud word cloud
#' @export
#' @examples
#' chart <- wordcloud(QOPEN_END)
#' chart <- wordcloud(QOPEN_END, '#FAEFF2')

wordcloud2 <- function(
  dataset,
  variable,
  colors = '#474E7E'
) {
  flag <- dplyr::enquo(variable)
  wordcloud <- openend(dataset, !!flag)
  ggplot2::ggplot(
    wordcloud,
    ggplot2::aes(
      x = 1,
      y = 1,
      size = n
    )
  ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        label = label,
        color = variable
      ),
      segment.size = 0
    ) +
    ggplot2::scale_color_manual(values = colors, guide = F) +
    ggplot2::scale_size(range = c(1, 12), guide = FALSE) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}
