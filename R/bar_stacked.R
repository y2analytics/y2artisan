#### Bar Stacked ####
### Description
#' Create a groupedbar mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies; The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'group_var'; For a single stacked bar, use
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = 'label'; When using the freqs function, will typically be label (is by default). All levels of the group_var must be present or the chart may break. To do this, save the variable as_factor() before running freqs. Also remember that label_text and label_color must exactly match all the levels of the group_var or the function will break.
#' @param direction DEFAULT = 'horizontal'; Two options: "horizontal" (default) OR "vertical"
#' @param label_text DEFAULT = 'text_settings_stacked'; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings_grouped <- list('Name of Group 1' = fp_text(font.size = 16, color = lime),'Name of Group 2' = fp_text(font.size = 16, color = brightblue))
#' @param label_color DEFAULT = 'color_settings_stacked'; A list of color settings for the levels within each stacked bar. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings_grouped <- list('Name of Group 1' = lime,'Name of Group 2' = brightblue)
#' @param label_show_values DEFAULT = T; TRUE or FALSE. Show percent labels for each value.
#' @param label_show_percent DEFAULT = F
#' @param label_position DEFAULT = 'outEnd'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt DEFAULT = '0\%'; Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt DEFAULT = '0\%\%'; Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_text_color DEFAULT = 'black'; Set to 'transparent' for no text on single bars
#' @param axis_y_label DEFAULT = ''; Title for the y_axis
#' @param axis_y_rotate DEFAULT = 0; Rotation of y_axis text. Set to -45 for diagonal, giving more space for text.
#' @param axis_y_rotate_title DEFAULT = 360, default for x_axis is 0
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = 1 to allow percent totals to add to 100\%.
#' @param axis_y_display DEFAULT = F
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param title_label DEFAULT = '; Add the question wording from the survey in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param gap_width DEFAULT = 150, meaning the size of the space between bars is 150\% the size of the bar itself
#' @param grouping DEFAULT = 'percentStacked'; grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlapping DEFAULT = 100
#' @param legend_pos DEFAULT = 't' for top; Other legend positions are 'b', 'tr', 'l', 'r', and 'n' for none.
#' @param legend_text_size DEFAULT = 10
#' @keywords chart stacked
#' @export
#' @examples
#' my_ms_chart <- bar_stacked()
#' OR for single stacked bar:
#' my_ms_chart <- bar_stacked(
#'   title_label  = 'Add the question wording from the survey here?',
#'   axis_x_text_color = 'transparent',
#'   x_var = 'variable'
#' )

bar_stacked <- function(
  data = frequencies,
  x_var = 'group_var',
  y_var = 'result',
  group_var = 'label',
  label_text = text_settings_stacked,
  label_color = color_settings_stacked,
  label_show_values = T,
  label_show_percent = F,
  label_position = 'outEnd',
  label_num_fmt = '0%',
  axis_num_fmt = '0%%',
  axis_x_text_color = 'black',
  axis_x_label = '',
  axis_x_display = T,
  axis_x_rotate = 0,
  axis_y_label = '',
  axis_y_min = 0,
  axis_y_max = 1,
  axis_y_display = F,
  axis_y_rotate = 0,
  axis_text_size = 14,
  axis_title_size = 18,
  title_label = '',
  title_size = 18,
  axis_y_rotate_title = 360,
  axis_x_rotate_title = 0,
  grouping = 'percentStacked',
  gap_width = 150,
  overlapping = 100,
  direction = 'horizontal',
  legend_text_size = 10,
  legend_pos = 't'
){
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
      show_percent = label_show_percent
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
      main_title = fp_text(font.size = title_size),
      axis_text_x = fp_text(font.size = axis_text_size, color = axis_x_text_color),
      axis_text_y = fp_text(color = 'transparent'),
      axis_title_x = fp_text(font.size = axis_title_size),
      axis_title_y = fp_text(font.size = axis_title_size),
      legend_text = fp_text(font.size = legend_text_size),
      grid_major_line_x = fp_border(style = 'none'),
      grid_major_line_y = fp_border(style = 'none'),
      title_y_rot = axis_y_rotate_title,
      title_x_rot = axis_x_rotate_title
    )  %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}

