#### gg_single ####
### Description
#' Create an ugrouped ggplot object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies;The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = label; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param label_var DEFAULT = percent_label; When using the order_label function, this variable will be created for you.
#' @param color_var DEFAULT = label; Although the color_var is set to label, the default for the "fills" argument sets all bars to be the same color.
#' @param axis_text_size DEFAULT = 12; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 14; Font size for x_label and y_label.
#' @param bar_width DEFAULT = .75, with a bar_width of 1 meaning each bars touches the ones next to it
#' @param direction DEFAULT = 'vertical'; Two options: "vertical" (default) OR "horizontal"
#' @param fills DEFAULT = rep('#474E7E', count(data)); the hexcode is the bluepurple color from the Qualtrics logo. This short function will give all the bars the same color. If colors are changed here in conjunction with the color_var, you can more easily manipulate colors based on another variable.
#' @param label_length DEFAULT = 45 for horizontal charts and 15 for vertical charts
#' @param label_size DEFAULT = 10
#' @param legend_pos DEFAULT = 'none'
#' @param legend_text_size DEFAULT = 6
#' @param legend_title DEFAULT = element_blank()
#' @param nudge DEFAULT = 0; however, nudge automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @param text_family DEFAULT = 'flama'
#' @param title_label DEFAULT = ''; Add the question wording from the survey in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param y_label DEFAULT = ''; Title for the y_axis
#' @param y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param y_max DEFAULT = 0; however, the y_max automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' my_ms_chart <- bar_single()
#' OR
#' my_ms_chart <- bar_single(
#'   rotate = -45,
#'   title_label  = 'Add the question wording from the survey here?',
#'   group_var = 'label',
#'   overlap = 100
#' )

gg_single <- function(
  data = frequencies,
  x_var = label,
  y_var = result,
  label_var = percent_label,
  color_var = label,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  direction = 'vertical',
  fills = rep('#474E7E', count(data)),
  label_length = 45,
  label_size = 10,
  legend_pos = 'none',
  legend_rev = FALSE,
  legend_text_size = 6,
  legend_title = element_blank(),
  nudge = 0, #auto-fills
  text_family = 'flama',
  title_label = '',
  title_size = 14,
  x_label = '',
  y_label = '',
  y_min = 0,
  y_max = 0 #auto-fills
) {
  #Flags
  x_flag <- dplyr::enquo(x_var)
  y_flag <- dplyr::enquo(y_var)
  color_flag <- dplyr::enquo(color_var)
  label_flag <- dplyr::enquo(label_var)
  max_y_val <- max(data$result)
  max_str_length <- data %>% select(!!x_flag) %>% as_vector() %>% str_length() %>% max()
  str_add <- max_str_length * max_y_val /1500

  y_max <- dplyr::case_when(
    y_max != 0 ~ y_max,
    # y_max == 0 & direction == 'horizontal' ~ (max_y_val + max_y_val/10 + str_add),
    T ~  (max_y_val + max_y_val/10) #direction == 'vertical'
  )
  nudge <- dplyr::case_when(
    nudge != 0 ~ nudge,
    direction == 'horizontal' ~ (max_y_val/20 + str_add),
    direction == 'vertical' ~ (max_y_val/16)
  )
  label_length <- dplyr::case_when(
    label_length != 45 ~ label_length,
    direction == 'vertical' ~ 15,
    T ~ label_length
  )


  #Chart
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
      guide = guide_legend(reverse = legend_rev),
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
      y = y_label
    ) +
    ggplot2::theme(
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size),
      legend.position = legend_pos,
      legend.spacing.x = unit(2, 'mm'),
      legend.text = element_text(size = legend_text_size),
      legend.title = legend_title,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = title_size)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      labels = function(x) str_c((round(x,2)) * 100, '%')
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
    if(direction == 'horizontal'){
      ggplot2::coord_flip()
    }
}


