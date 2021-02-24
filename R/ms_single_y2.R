#### Bar Single ####
### Description
#' Create an ugrouped mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies;The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'label'; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = NULL; If you want the bars to be different colors, set group_var to the same variable as x_var. Then set overlap to 100.
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param axis_x_position DEFAULT = 'nextTo'; Other options include "high", "low", "none". Change to "low" if dealing with negative numbers
#' @param axis_x_display,axis_y_display DEFAULT = TRUE
#' @param axis_x_label,axis_y_label DEFAULT = ''; Title for the x_axis and y_axis
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = NULL
#' @param axis_x_rotate,axis_y_rotate DEFAULT = 0; Rotation of axis text. Set to -45 for diagonal, giving more space for text.
#' @param axis_x_rotate_title,axis_y_rotate_title DEFAULT = 0, set y_axis rotation to 360 for horizontal text
#' @param direction DEFAULT = 'vertical'; Two options: "vertical" (default) OR "horizontal"
#' @param font_family DEFAULT = 'Arial'. Sets the fonts for axis, legends, and titles. Label font is set within label_color and label_text lists. May specify fonts in quotes, e.g. "Times New Roman"
#' @param gap_width DEFAULT = 25, meaning the size of the space between bars is 25\% the size of the bar itself
#' @param grouping DEFAULT = 'standard'; grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param label_text DEFAULT = text_settings; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings <- list("result" = fp_text(font.size = 10.5, color = bluepurple, font.family = 'Arial'))
#' @param label_color DEFAULT = color_settings; A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings <- list(bluepurple)
#' @param label_show_values DEFAULT = TRUE; TRUE or FALSE. Show percent labels for each value.
#' @param label_position DEFAULT = 'outEnd'; Other options include c('outEnd', 'inEnd', 'ctr', 'inBase')
#' @param legend_pos DEFAULT = 'n' for none. Other legend positions are 'b', 'tr', 'l', 'r', 't'.
#' @param legend_text_size DEFAULT = 16
#' @param num_fmt DEFAULT = 'percent'; Can also be set to 'general' for non-percentages. Changes formatting for both the labels and axis
#' @param overlapping DEFAULT = -50; This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @importFrom officer fp_text fp_border
#' @keywords chart bar single
#' @export
#' @examples
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = TRUE)
#'
#' color_settings <- list('blue')
#' text_settings<- list('result' = fp_text(font.size = 20))
#'
#' chart <- ms_single_y2()
#' print(chart, preview = TRUE)



ms_single_y2 <-  function(
  data = frequencies,
  x_var = 'label',
  y_var = 'result',
  axis_text_size = 14,
  axis_title_size = 18,
  axis_x_display = TRUE,
  axis_x_label = '',
  axis_x_position = c('nextTo', 'high', 'low', 'none'),
  axis_x_rotate = 0,
  axis_x_rotate_title = 0,
  axis_y_display = TRUE,
  axis_y_label = '',
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_rotate = 0,
  axis_y_rotate_title = 0,
  direction = c('vertical', 'horizontal'),
  font_family = 'Arial',
  gap_width = 25,
  grouping = 'standard',
  group_var = NULL,
  label_color = color_settings,
  label_position = c('outEnd', 'inEnd', 'ctr', 'inBase'),
  label_show_values = TRUE,
  label_text = text_settings,
  legend_pos = c('n', 't', 'b', 'tr', 'l', 'r'),
  legend_text_size = 16,
  num_fmt = c('percent', 'general'),
  overlapping = -50,
  title_label = '',
  title_size = 18
) {

### Check for special symbols
  freqs_list <- split(data, seq(nrow(data))) # turn data frame into a list
  symbols_sum <- purrr::map_df(freqs_list, ~stringr::str_detect(.x, "<|&")) %>% # test if any cells contain special symbols
    dplyr::mutate_all(~as.numeric(.)) %>% # convert table into numerics
    sum() # sum all cells to count the number of special symbols
  if(symbols_sum > 0){
    stop('mschart objects cannot contain the special symbols "&" or "<". Please remove those symbols from your data frame')
  }



### Flags
  axis_x_position <- rlang::arg_match(axis_x_position)
  direction <- rlang::arg_match(direction)
  legend_pos <- rlang::arg_match(legend_pos)
  label_position <- rlang::arg_match(label_position)
  num_fmt <- rlang::arg_match(num_fmt)
  axis_num_fmt <- dplyr::case_when(
    num_fmt == 'percent' ~ '0%%',
    num_fmt == 'general' ~ 'general'
  )
  label_num_fmt <- dplyr::case_when(
    num_fmt == 'percent' ~ '0%',
    num_fmt == 'general' ~ 'general'
  )



### Chart
  mschart::ms_barchart(
    data,
    x = x_var,
    y = y_var,
    group = group_var
  ) %>%
    mschart::chart_settings(
      dir = direction,
      grouping = grouping,
      overlap = overlapping,
      gap_width = gap_width
    ) %>%
    mschart::chart_data_labels(
      show_val = label_show_values,
      num_fmt = label_num_fmt,
      position = label_position
    ) %>%
    mschart::chart_labels_text(
      values = label_text
    ) %>%
    mschart::chart_data_fill(
      values = label_color
    ) %>%
    mschart::chart_data_stroke(
      values = label_color
    ) %>%
    mschart::chart_ax_y(
      num_fmt = axis_num_fmt,
      limit_min = axis_y_min,
      limit_max = axis_y_max
    ) %>%
    mschart::chart_ax_x(
      rotation = axis_x_rotate
    ) %>%
    mschart::chart_ax_y(
      rotation = axis_y_rotate
    ) %>%
    mschart::chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    mschart::chart_theme(
      legend_position = legend_pos,
      main_title = fp_text(font.size = title_size, font.family = font_family),
      axis_text_x = fp_text(font.size = axis_text_size, font.family = font_family),
      axis_text_y = fp_text(font.size = axis_text_size, font.family = font_family),
      axis_title_x = fp_text(font.size = axis_title_size, font.family = font_family),
      axis_title_y = fp_text(font.size = axis_title_size, font.family = font_family),
      legend_text = fp_text(font.size = legend_text_size, font.family = font_family),
      grid_major_line_x = fp_border(width = 0),
      grid_major_line_y = fp_border(width = 0),
      title_y_rot = axis_y_rotate_title,
      title_x_rot = axis_x_rotate_title
    ) %>%
    mschart::chart_ax_x(
      display = axis_x_display,
      tick_label_pos = axis_x_position
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}


