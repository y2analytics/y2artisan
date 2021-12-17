#### Final Dot Plot Function ####
### Description
#' Create a grouped ggplot object
#'
#' This function creates a ggplot2 object automatically formatted for a dot plot with a grouping variable.
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param y_var DEFAULT = label; When using the freqs function, will typically be label (is by default).
#' @param color_var DEFAULT = group_var. Note: the color variable CANNOT be numeric.
#' @param axis_text_size DEFAULT = 12; Font size for variable levels and axis percentages.
#' @param axis_title_size DEFAULT = 14; Font size for x_label and y_label.
#' @param direction DEFAULT = 'horizontal'; Two options: "horizontal" (default) OR "vertical"
#' @param fills DEFAULT = 'NULL'; requires a vector of colors for all levels of the color_var/grouping variable. Otherwise, it will run fills as default ggplot2 colors.
#' @param font_family DEFAULT = 'flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param label_length DEFAULT = 45 for horizontal charts and 15 for vertical charts. This determines how many characters an x-axis label can be before R inserts a line break.
#' @param legend_nrow DEFAULT = NULL; Change to a numeric to specify the number of rows for the legend
#' @param legend_pos DEFAULT = 'top'
#' @param legend_rev DEFAULT = FALSE
#' @param legend_text_size DEFAULT = 8
#' @param legend_title_size DEFAULT = 8
#' @param legend_title DEFAULT = '', If you put in a title, the legend will default to 'top' unless otherwise specified
#' @param point_size DEFAULT = 6; Size for each point in the dot plot.
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param x_label,y_label DEFAULT = ''; Title for the x_axis or y_axis
#' @param x_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param x_max DEFAULT = 0; however, the y_max automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @keywords chart ggplot dot point
#' @export
#' @examples
#' frequencies <- mpg %>%
#'   dplyr::group_by(manufacturer) %>%
#'   y2clerk::freqs(cty, hwy, stat = 'mean') %>%
#'   dplyr::mutate(label = variable) %>%
#'   orderlabel::order_label(
#'     group_var = group_var,
#'     inherent_order_group = TRUE
#'     )
#' chart <- gg_dotplot_y2(
#'   fills = c(
#'     "#F8766D", "#E58700", "#C99800", "#A3A500",
#'     "#6BB100", "#00BA38", "#00BF7D", "#00C0AF",
#'     "#00BCD8", "#00B0F6", "#619CFF", "#B983FF",
#'     "#E76BF3", "#FD61D1", "#FF67A4", "#F8766D"
#'   ),
#'   font_family = 'sans'
#' )
#' chart

gg_dotplot_y2 <- function(
  data = frequencies,
  x_var = result,
  y_var = label,
  color_var = group_var,
  axis_text_size = 12,
  axis_title_size = 14,
  direction = c('horizontal', 'vertical'),
  fills = 'NULL',
  font_family = 'flama',
  label_length = 45,
  legend_pos = 'top',
  legend_nrow = NULL,
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = '',
  point_size = 6,
  title_label = '',
  title_size = 14,
  x_label = '',
  y_label = '',
  x_min = 0,
  x_max = 0 # auto-fills
) {
  ### Set defaults
  label <- result <- group_var <- NULL
  direction <- rlang::arg_match(direction)

  max_x_value <- data %>% dplyr::summarise(max({{x_var}})) %>% as.numeric()
  max_str_length <- data %>% dplyr::select({{y_var}}) %>% purrr::as_vector() %>% stringr::str_length() %>% max()
  str_add <- max_str_length * max_x_value / 1500
  x_max <- dplyr::case_when(
    x_max != 0 ~ x_max,
    TRUE ~  (max_x_value + max_x_value / 10) #direction == 'vertical'
  )


  ### Conditional parts
  cond_colors <- if (fills[1] != 'NULL') {
    ggplot2::scale_color_manual(
      guide = ggplot2::guide_legend(
        reverse = legend_rev,
        nrow = legend_nrow,
        title = legend_title
      ),
      values = fills
    )
  } else {NULL}

  ### Main function
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data,
      mapping = ggplot2::aes(
        x = {{ x_var }},
        y = {{ y_var }},
        color = {{ color_var }}
      ),
      size = point_size
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) lapply(
        strwrap(
          x,
          width = label_length,
          simplify = FALSE
        ),
        paste,
        collapse="\n"
      )
    )  +
    ggplot2::ggtitle(
      title_label
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_blank(),
      legend.position = 'top',
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      panel.grid.major = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = title_size),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(x_min, x_max),
      labels = scales::percent
    ) +
    cond_colors
}


