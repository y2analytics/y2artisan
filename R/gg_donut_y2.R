#### gg_donut_y2 ####
### Description
#' Create a donut chart ggplot object
#'
#' This function creates a ggplot2 object automatically formatted for a donut chart
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from
#' @param color_var DEFAULT = label; Note: the color variable CANNOT be numeric
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default)
#' @param center_label DEFAULT = 'NULL'. When set to 'NULL', each individual level of the donut chart will have a label over its respective section. If set to a specific value of the color_var, that percentage will appear in the center, without a label, and no other labels will appear on the donut chart.
#' @param colors DEFAULT is white ('#ffffff') for the text of all percent labels; You may also 1) Specify 1 color, and this color will be applied to all color_var levels or 2) Specify a vector of colors for each individual level of the color_var
#' @param fills NO DEFAULT; requires a vector of colors for all levels of the x_var
#' @param font_family DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param hole_size DEFAULT = 2.5. The larger the size, the larger the donut hole (minimum of 1; no maximum but higher than 10 is not recommended)
#' @param label_size DEFAULT = 10. Adjusts the size of the percent labels
#' @param legend_pos DEFAULT = 'none'
#' @param legend_rev DEFAULT = FALSE
#' @param legend_text_size DEFAULT = 8
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart
#' @param title_size DEFAULT = 40
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' # Each level of the chart has a label
#' frequencies <- iris %>% y2clerk::freqs(Species)
#' chart <- gg_donut_y2(
#'   fills = c('red', 'blue', 'pink'),
#'   font_family = 'sans'
#' )
#'
#' # Only one level of the chart has a percentage, in the center of the donut
#' frequencies <- ToothGrowth %>% y2clerk::freqs(supp)
#' chart <- gg_donut_y2(
#'   fills = c('orange', 'gray'),
#'   center_label = 'OJ',
#'   font_family = 'sans',
#'   title_label = 'OJ',
#'   label_size = 15,
#' )

gg_donut_y2 <- function(
    data = frequencies,
    color_var = label,
    y_var = result,
    center_label = 'NULL',
    colors = '#ffffff',
    fills, #only variable with no default...
    font_family = 'Flama',
    hole_size = 2.5,
    label_size = 5,
    legend_pos = 'none',
    legend_rev = FALSE,
    legend_text_size = 8,
    title_label = '',
    title_size = 40
) {

  ### Pre-chart
  if (
    (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
  ) {
    stop("The font you specified in the 'font_family' argument does not exist in your R session")
  }

  label <- result <- percent_label <- NULL

  colors <- if (length(colors) == 1) { # If user specifies only one color, repeat color for all bars
    colors <- rep(colors, dplyr::count(data))
  } else {
    colors <- colors
  }

  ### center_label
  if (center_label == 'NULL') {
    label_block <- ggplot2::geom_text(
      ggplot2::aes(label = percent_label),
      family = font_family,
      size = label_size,
      color = colors,
      position = ggplot2::position_fill(vjust = 0.5)
    )
  } else {
    label_block <- ggplot2::geom_text(
      ggplot2::aes(
        x = .5,
        y = 0,
        label = percent_label,
      ),
      color = fills[1],
      size = label_size
    )
  }

  if ( center_label == 'NULL') {
  data <- data %>%
    dplyr::mutate(
      percent_label = stringr::str_c(
        {{ color_var }},
        '\n',
        {{ y_var }} * 100,
        '%'
      )
    )
  } else {
    data <- data %>%
      dplyr::mutate(
        percent_label = dplyr::case_when(
          {{ color_var }} == center_label ~
            stringr::str_c({{ y_var }} * 100, '%'),
          TRUE ~ ''
        )
      )
  }
  ### end center label section

  if (legend_pos != 'none') {
    data <- data %>%
      dplyr::mutate(
        percent_label = stringr::str_c(
          {{ y_var }} * 100,
          '%'
        )
      )
  } else {
    data <- data
    }


  ### Chart
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = hole_size,
      y = {{ y_var }},
      fill = {{ color_var }}
    )
  ) +
    ggplot2::geom_col(position = 'fill') +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(c(.5, hole_size + .5)) +
    label_block +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(reverse = legend_rev),
      values = fills
    ) +
    ggplot2::scale_color_manual(
      guide = 'none',
      values = colors
    ) +
    ggplot2::ggtitle(
      title_label
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      fill = ""
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = title_size,
        hjust = .5,
        color = fills[1]
        ),
      text = ggplot2::element_text(family = font_family)
    )
}

