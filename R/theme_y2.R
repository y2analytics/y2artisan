#### theme_y2 ####
### Description
#' Add the classic y2 theme to any ggplot2 chart
#'
#' All y2artisan charts already have the y2 theme built in, but if you're creating a more custom ggplot2 chart and want to add the classic y2 theme, use theme_y2()
#' @keywords chart ggplot theme
#' @export
#' @examples
#' \dontrun{
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = TRUE)
#'
#' chart <- ggplot2::ggplot(
#'   frequencies,
#'   ggplot2::aes(x = label, y = result)
#' ) +
#'   ggplot2::geom_bar(stat = 'identity') +
#'   theme_y2()
#'   }

theme_y2 <- function() {
  ### Check fonts
  if(
    (stringr::str_detect(sysfonts::font_families(), 'flama') %>% sum == 0)
  ){
    stop("The font 'flama' - required for the y2 theme - does not exist in your R session")
  }

  ### Theme function
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    legend.spacing.x = ggplot2::unit(2, 'mm'),
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 8),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 14),
    text = ggplot2::element_text(family = 'flama')
  )
}

