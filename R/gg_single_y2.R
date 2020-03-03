#### gg_single_y2 ####
### Description
#' Create an ungrouped ggplot object
#'
#' This function creates a ggplot2 object automatically formatted for a single variable (including multiple select).
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = label; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param label_var DEFAULT = percent_label; When using the order_label function, this variable will be created for you.
#' @param color_var DEFAULT = label; Although the color_var is set to label, the default for the "fills" argument sets all bars to be the same color if only a single color is specified. Note: the color variable CANNOT be numeric.
#' @param axis_text_size DEFAULT = 12; Font size for variable levels and axis percentages.
#' @param axis_title_size DEFAULT = 14; Font size for x_label and y_label.
#' @param bar_width DEFAULT = .75, with a bar_width of 1 meaning each bars touches the ones next to it
#' @param chart_height DEFAULT = 5.5, If saving out a vertical bar chart with a different height, set the height here to have the nudge argument adjust itself automatically
#' @param chart_width DEFAULT = 11, If saving out a horizontal bar chart with a different width, set the width here to have the nudge argument adjust itself automatically
#' @param direction DEFAULT = 'vertical'; Two options: "vertical" (default) OR "horizontal"
#' @param fills DEFAULT = '#474E7E', the purple Qualtrics color. Three options: 1) Give only 1 color, all bars will be that color. 2) Specify the color for every bar using a vector of colors. 3) Set the "color_var" argument to a variable in the data frame and specify a color for each level in the color_var.
#' @param label_length DEFAULT = 45 for horizontal charts and 15 for vertical charts. This determines how many characters an x-axis label can be before R inserts a line break.
#' @param label_size DEFAULT = 10. Adjusts the size of the percent labels over each bar.
#' @param legend_pos DEFAULT = 'none'
#' @param legend_text_size DEFAULT = 6
#' @param legend_title_size DEFAULT = 8
#' @param legend_title DEFAULT = '', If you put in a title, the legend will default to 'top' unless otherwise specified
#' @param nudge DEFAULT = 0; however, nudge automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @param text_family DEFAULT = 'flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param y_label DEFAULT = ''; Title for the y_axis
#' @param y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param y_max DEFAULT = 0; however, the y_max automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#'
#' chart <- gg_single_y2()
#'
#' # When you change the color, you must specify a level for each bar. Here's an easy way to do so if they're all the same color
#' chart <- gg_single_y2(fills = rep('pink', count(frequencies)))
#'

gg_single_y2 <- function(
  data = frequencies,
  x_var = label,
  y_var = result,
  label_var = percent_label,
  color_var = label,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  chart_height = 5.5,
  chart_width = 11,
  direction = c('vertical', 'horizontal'),
  fills = '#474E7E',
  label_length = 45,
  label_size = 10,
  legend_pos = 'none',
  legend_rev = FALSE,
  legend_text_size = 6,
  legend_title_size = 8,
  legend_title = '',
  nudge = 0, #auto-fills
  text_family = 'flama',
  title_label = '',
  title_size = 14,
  x_label = '',
  y_label = '',
  y_min = 0,
  y_max = 0 #auto-fills
) {

### Flags
  x_flag <- dplyr::enquo(x_var)
  y_flag <- dplyr::enquo(y_var)
  color_flag <- dplyr::enquo(color_var)
  label_flag <- dplyr::enquo(label_var)
  direction <- rlang::arg_match(direction)



### Set defaults
  max_y_val <- data %>% dplyr::summarise(max(!!y_flag)) %>% as.numeric()
  max_str_length <- data %>% dplyr::select(!!x_flag) %>% purrr::as_vector() %>% stringr::str_length() %>% max()
  str_add <- max_str_length * max_y_val /1500
  legend_pos <- dplyr::case_when(
    legend_title != '' & legend_pos == 'none' ~ 'top',
    T ~ legend_pos
  )
  y_max <- dplyr::case_when(
    y_max != 0 ~ y_max,
    # y_max == 0 & direction == 'horizontal' ~ (max_y_val + max_y_val/10 + str_add),
    chart_width < 11 & direction == 'horizontal' ~  (max_y_val + max_y_val/chart_width),
    chart_height < 5.5 & direction == 'vertical' ~  (max_y_val + max_y_val/(chart_height*2)),
    T ~  (max_y_val + max_y_val/10) #direction == 'vertical'
  )
  nudge <- dplyr::case_when(
    nudge != 0 ~ nudge, #if user specifies nudge, don't change it
    direction == 'horizontal' ~ (max_y_val/20 + str_add),
    direction == 'vertical' ~ (max_y_val/16)
  )
  nudge <- dplyr::case_when(
    chart_width != 11 & direction == 'horizontal' ~ nudge/(chart_width/12),
    chart_height != 5.5 & direction == 'vertical' ~ nudge/(chart_height/6),
    T ~ nudge # If chart width is default of 11, then should be good
  )
  label_length <- dplyr::case_when(
    label_length != 45 ~ label_length,
    direction == 'vertical' ~ 15,
    T ~ label_length
  )
  fills <- if(length(fills) == 1) { # If user specifies only one color, repeat color for all bars
    fills <- rep(fills, dplyr::count(data))
  } else{
    fills <- fills
  }



### Conditional chunks
  cond_direction <- if(direction == 'horizontal'){
  ggplot2::coord_flip()
} else{NULL}



### Chart
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!x_flag,
      y = !!y_flag
    )
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(
        fill = !!color_flag
      ),
      stat = 'identity',
      width = bar_width
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = !!label_flag,
        color = !!color_flag
        ),
      family = text_family,
      size = label_size,
      nudge_y = nudge
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(reverse = legend_rev),
      values = fills
    ) +
    ggplot2::scale_color_manual(
      guide = FALSE,
      values = fills
    ) +
    ggplot2::ggtitle(
      title_label
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      legend.position = legend_pos,
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = title_size),
      text = element_text(family = text_family)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      labels = function(x) stringr::str_c((round(x,2)) * 100, '%')
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) lapply(
        strwrap(
          x,
          width = label_length,
          simplify = FALSE
        ),
        paste,
        collapse="\n"
      )
    ) +
    cond_direction

}


