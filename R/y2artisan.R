#### ***** MS CHARTS ***** ####
#### Bar Single ####
### Description
#' Create an ugrouped mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies;The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'label'; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = NULL; If you want the bars to be different colors, set group_var to the same variable as x_var. Then set overlap to 100.
#' @param label_text DEFAULT = 'text_settings'; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings <- list(fp_text(font.size = 10.5, color = bluepurple))
#' @param label_color DEFAULT = 'color_settings'; A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings <- list(bluepurple)
#' @param label_show_values DEFAULT = T; TRUE or FALSE. Show percent labels for each value.
#' @param label_position DEFAULT = 'outEnd'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt DEFAULT = '0\%'; Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt DEFAULT = '0\%\%'; Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label DEFAULT = ''; Title for the x_axis
#' @param axis_y_label DEFAULT = ''; Title for the y_axis
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = NULL
#' @param axis_y_display DEFAULT = T
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param title_label DEFAULT = ''; Add the question wording from the survey in "" as the title of the chart.
#' @param rotate DEFAULT = 0; Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping DEFAULT = 'standard'; grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param gap_width DEFAULT = 150, meaning the size of the space between bars is 150\% the size of the bar itself
#' @param overlapping DEFAULT = -50; This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param direction DEFAULT = 'vertical'; Two options: "vertical" (default) OR "horizontal"
#' @param legend_text_size DEFAULT = 16
#' @param legend_pos DEFAULT = 'n' for none. Other legend positions are 'b', 'tr', 'l', 'r', 't'.
#' @keywords chart
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


bar_single <-  function(
  data = frequencies,
  x_var = 'label',
  y_var = 'result',
  group_var = NULL,
  label_text = text_settings,
  label_color = color_settings,
  label_show_values = T,
  label_position = 'outEnd',
  label_num_fmt = '0%',
  axis_num_fmt = '0%%',
  axis_x_label = '',
  axis_y_label = '',
  axis_x_display = T,
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_display = T,
  axis_text_size = 14,
  axis_title_size = 18,
  title_label = '',
  title_size = 18,
  rotate = 0,
  grouping = 'standard',
  gap_width = 150,
  overlapping = -50,
  direction = 'vertical',
  legend_text_size = 16,
  legend_pos = 'n'
) {
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
      rotation = rotate
    ) %>%
    mschart::chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    mschart::chart_theme(
      legend_position = legend_pos,
      main_title = fp_text(font.size = title_size),
      axis_text_x = fp_text(font.size = axis_text_size),
      axis_text_y = fp_text(font.size = axis_text_size),
      axis_title_x = fp_text(font.size = axis_title_size),
      axis_title_y = fp_text(font.size = axis_title_size),
      legend_text = fp_text(font.size = legend_text_size),
      grid_major_line_x = fp_border(width = 0),
      grid_major_line_y = fp_border(width = 0)
    ) %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}


#### Bar Grouped ####
### Description
#' Create a groupedbar mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies; The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'label'; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = 'group_var'; All levels of the group_var must be present or the chart may break. To do this, save the variable as_factor() before running freqs. Also remember that label_text and label_color must exactly match all the levels of the group_var or the function will break.
#' @param label_text DEFAULT = 'text_settings_grouped'; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings_grouped <- list('Name of Group 1' = fp_text(font.size = 16, color = lime),'Name of Group 2' = fp_text(font.size = 16, color = brightblue))
#' @param label_color DEFAULT = 'color_settings_grouped'; A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings_grouped <- list('Name of Group 1' = lime,'Name of Group 2' = brightblue)
#' @param label_show_values DEFAULT = T; TRUE or FALSE. Show percent labels for each value.
#' @param label_position DEFAULT = 'outEnd'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt DEFAULT = '0\%'; Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt DEFAULT = '0\%\%'; Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label DEFAULT = ''; Title for the x_axis
#' @param axis_x_display DEFAULT = T
#' @param axis_y_label DEFAULT = ''; Title for the y_axis
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = NULL
#' @param axis_y_display DEFAULT = T
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param title_label DEFAULT = ''; Add the question wording from the survey in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param rotate DEFAULT = 0; Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping DEFAULT = 'standard'; grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param gap_width DEFAULT = 150, meaning the size of the space between bars is 150\% the size of the bar itself
#' @param overlapping DEFAULT = -50 This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param direction DEFAULT = 'vertical'; Two options: "vertical" (default) OR "horizontal"
#' @param legend_pos DEFAULT = 't' for top; Other legend positions are 'b', 'tr', 'l', 'r', and 'n' for none.
#' @param legend_text_size DEFAULT = 16
#' @keywords chart grouped
#' @export
#' @examples
#' my_ms_chart <- bar_grouped()
#' OR
#' my_ms_chart <- bar_grouped(
#'   rotate = -45,
#'   title_label  = 'Add the question wording from the survey here?',
#'   group_var = 'district',
#'   direction = 'horizontal
#' )

bar_grouped <-  function(
  data = frequencies,
  x_var = 'label',
  y_var = 'result',
  group_var = 'group_var',
  label_text = text_settings_grouped,
  label_color = color_settings_grouped,
  label_show_values = T,
  label_position = 'outEnd',
  label_num_fmt = '0%',
  axis_num_fmt = '0%%',
  axis_y_label = '',
  axis_x_label = '',
  axis_x_display = T,
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_display = T,
  axis_text_size = 14,
  axis_title_size = 18,
  title_label = '',
  title_size = 18,
  rotate = 0,
  grouping = 'standard',
  gap_width = 150,
  overlapping = -50,
  direction = 'vertical',
  legend_text_size = 16,
  legend_pos = 't'
) {
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
      rotation = rotate
    ) %>%
    mschart::chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    mschart::chart_theme(
      legend_position = legend_pos,
      main_title = fp_text(font.size = title_size),
      axis_text_x = fp_text(font.size = axis_text_size),
      axis_text_y = fp_text(font.size = axis_text_size),
      axis_title_x = fp_text(font.size = axis_title_size),
      axis_title_y = fp_text(font.size = axis_title_size),
      legend_text = fp_text(font.size = legend_text_size),
      grid_major_line_x = fp_border(width = 0),
      grid_major_line_y = fp_border(width = 0)
    ) %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}

#### Bar Stacked ####
### Description
#' Create a groupedbar mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies; The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'group_var'; For a single stacked bar, use
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = 'label'; When using the freqs function, will typically be label (is by default). All levels of the group_var must be present or the chart may break. To do this, save the variable as_factor() before running freqs. Also remember that label_text and label_color must exactly match all the levels of the group_var or the function will break.
#' @param label_text DEFAULT = 'text_settings_stacked'; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings_grouped <- list('Name of Group 1' = fp_text(font.size = 16, color = lime),'Name of Group 2' = fp_text(font.size = 16, color = brightblue))
#' @param label_color DEFAULT = 'color_settings_stacked'; A list of color settings for the levels within each stacked bar. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings_grouped <- list('Name of Group 1' = lime,'Name of Group 2' = brightblue)
#' @param label_show_values DEFAULT = T; TRUE or FALSE. Show percent labels for each value.
#' @param label_show_percent DEFAULT = F
#' @param label_position DEFAULT = 'outEnd'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt DEFAULT = '0\%'; Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt DEFAULT = '0\%\%'; Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_text_color DEFAULT = 'black'; Set to 'transparent' for no text on single bars
#' @param axis_x_label DEFAULT = ''; Title for the x_axis
#' @param axis_y_label DEFAULT = ''; Title for the y_axis
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = 1 to allow percent totals to add to 100%.
#' @param axis_y_display DEFAULT = F
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param title_label DEFAULT = '; Add the question wording from the survey in "" as the title of the chart.
#' @param rotate DEFAULT = 0; Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping DEFAULT = 'percentStacked'; grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param gap_width DEFAULT = 150, meaning the size of the space between bars is 150\% the size of the bar itself
#' @param overlapping DEFAULT = 100
#' @param direction DEFAULT = 'horizontal'; Two options: "horizontal" (default) OR "vertical"
#' @param legend_text_size DEFAULT = 10
#' @param legend_pos DEFAULT = 't' for top; Other legend positions are 'b', 'tr', 'l', 'r', and 'n' for none.
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
  axis_y_label = '',
  axis_y_min = 0,
  axis_y_max = 1,
  axis_y_display = F,
  axis_text_size = 14,
  axis_title_size = 18,
  title_label = '',
  title_size = 18,
  rotate = 0,
  rotate_axis_y_title = 360,
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
      rotation = rotate
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
      title_y_rot = rotate_axis_y_title
    )  %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}

#### Line chart ####
#' Create an ugrouped mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data DEFAULT = frequencies;The name of the data frame that the mscharts pulls from.
#' @param x_var DEFAULT = 'year'; The name of your time variable
#' @param y_var DEFAULT = 'result'; When using the freqs function, will typically be result (is by default).
#' @param group_var DEFAULT = 'label'; Each line will be a distinct value of this variable
#' @param label_text DEFAULT = 'text_settings'; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings <- list(fp_text(font.size = 10.5, color = bluepurple))
#' @param label_color DEFAULT = 'color_settings'; A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings <- list('District 1' = lime, 'District 2' = bluepurple)
#' @param label_show_values DEFAULT = T; TRUE or FALSE. Show percent labels for each value.
#' @param label_position DEFAULT = 't'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt DEFAULT = '0\%'; Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt DEFAULT = '0\%\%'; Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label DEFAULT = ''; Title for the x_axis
#' @param axis_y_label DEFAULT = ''; Title for the y_axis
#' @param axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max DEFAULT = NULL
#' @param axis_y_display DEFAULT = T
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param title_label DEFAULT = ''; Add the question wording from the survey in "" as the title of the chart.
#' @param legend_text_size DEFAULT = 14
#' @param legend_pos DEFAULT = 'n' for none; Other legend positions are 'b', 'tr', 'l', 'r', 't'.
#' @keywords chart
#' @export
#' @examples
#' my_ms_chart <- line()
#' OR
#' my_ms_chart <- line(
#'   x_var = 'month',
#'   group_var = 'brand,
#'   title_label  = 'Title of my chart',
#' )


line_chart <- function(
  data = frequencies,
  x_var = 'year',
  y_var = 'result',
  group_var = 'label',
  label_text = text_settings,
  label_color = color_settings,
  label_show_values = T,
  label_position = 't',
  label_num_fmt = '0%',
  axis_num_fmt = '0%%',
  axis_x_display = T,
  axis_x_label = '',
  axis_y_label = '',
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_display = T,
  axis_text_size = 14,
  axis_title_size = 18,
  title_label = '',
  title_size = 18,
  rotate = 0,
  legend_text_size = 16,
  legend_pos = 'n'
){
  ms_linechart(
    data = data,
    x = x_var,
    y = y_var,
    group = group_var
  ) %>%
    chart_data_labels(
      show_val = label_show_values,
      num_fmt = label_num_fmt,
      position = label_position
    ) %>%
    chart_labels_text(
      values = label_text
    ) %>%
    chart_data_fill(
      values = label_color
    ) %>%
    chart_data_stroke(
      values = label_color
    ) %>%
    chart_ax_y(
      num_fmt = axis_num_fmt,
      limit_min = axis_y_min,
      limit_max = axis_y_max
    ) %>%
    chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    chart_theme(
      legend_position = legend_pos,
      main_title = fp_text(font.size = title_size),
      axis_text_x = fp_text(font.size = axis_text_size),
      axis_text_y = fp_text(font.size = axis_text_size),
      axis_title_x = fp_text(font.size = axis_title_size),
      axis_title_y = fp_text(font.size = axis_title_size),
      legend_text = fp_text(font.size = legend_text_size),
      grid_major_line_x = fp_border(width = 0),
      grid_major_line_y = fp_border(width = 0)
    ) %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    )
}

#### Add 1 Slide ####
### Description
#' Add 1 Blank PowerPoint slide
#'
#' This function adds a new PowerPoint slide along with a title and commentary box to the current pp object in R
#' @param slide_name DEFAULT: "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords powerpoint slide
#' @export
#' @examples
#' doc <- add1s()

add1s <- function(
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme"
) {
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
    )
  doc <- ph_with_text(
    doc,
    type = 'title',
    str = 'xxx'
  )
  doc <- ph_with_text(
    doc,
    type = 'body',
    str = 'xxx'
  )
}

#### Add section header ####
### Description
#' Add Blank PowerPoint Section Header
#'
#' This function adds a new PowerPoint section header along with a title box
#' @param title DEFAULT: "xxx"; Add a title in quotes here to have your Section Headers put in for you by R
#' @param slide_name DEFAULT: "Section Header"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords powerpoint slide
#' @export
#' @examples
#' doc <- add_section_header()

add_section_header <- function(
  title = 'xxx',
  slide_name = "Section Header",
  master_name = "Office Theme"
) {
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )
  doc <- ph_with_text(
    doc,
    type = 'title',
    str = title
  )
}

#### Add 1 Chart ####
### Description
#' Add PowerPoint slide & 1 chart
#'
#' This function adds a new PowerPoint slide and fits 1 chart onto it. It automatically fits the location to the center of the slide.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param add_slide DEFAULT = T; Automatically adds a blank slide for the chart to be added onto. If F, no new slide is added
#' @param slide_name DEFAULT: "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords chart
#' @export
#' @examples
#' doc <- add1c(my_ms_chart)
#' OR
#' doc <- add1c(my_ms_chart, slide_name = "1_Blank", master = "Custom Design")

### Function
add1c <- function(
  name,
  add_slide = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  if(add_slide == T) {
    doc <- add1s(slide_name, master_name)
  } else {
    doc <- doc
  }
  mschart::ph_with_chart_at(
    doc,
    chart = name,
    left = left_start,
    top = top_start,
    height = height,
    width = width)
}


#### Add 2 Charts ####
### Description
#' Add 2 charts to PowerPoint slide
#'
#' This function adds 2 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "top"; "bottom"; "left"; "right". The chart layout can either be top-bottom OR left-right.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'left')
#' doc <- add2c(chart_name, 'right')
#' OR
#' #' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'left', label_first_only = T)
#' doc <- add2c(chart_name, 'right', T)
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'top')
#' doc <- add2c(chart_name, 'bottom')

### Function
add2c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F) {
  mschart::ph_with_chart_at(
    doc,
    chart = name,
    left = dplyr::case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = dplyr::case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    ),
    height = dplyr::case_when(
      position == 'top' ~ 3,
      position == 'bottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = dplyr::case_when(
      position == 'top' ~ 12,
      position == 'bottom' ~ 12,
      position == 'left' ~ 6,
      position == 'right' ~ 6
    )
  )
  } else{ #label_first_only == T
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'top' ~ .5,
        position == 'bottom' ~ .5,
        position == 'left' ~ 0,
        position == 'right' ~ 8.25
      ),
      top = dplyr::case_when(
        position == 'top' ~ 2,
        position == 'bottom' ~ 4.5,
        position == 'left' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'top' ~ 3,
        position == 'bottom' ~ 3,
        position == 'left' ~ 5.5,
        position == 'right' ~ 5.5
      ),
      width = dplyr::case_when(
        position == 'top' ~ 12,
        position == 'bottom' ~ 12,
        position == 'left' ~ 8.25,
        position == 'right' ~ 4.75
      )
    )
  }
}

#### Add 3 Charts ####
### Description
#' Add 3 charts to PowerPoint slide
#'
#' This function adds 3 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "center"; "right"; "bottomright"; "topright". The chart layout can either be left-center-right OR left-topright-bottomright.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add3c(chart_name, 'left')
#' doc <- add3c(chart_name, 'center')
#' doc <- add3c(chart_name, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add3c(chart_name, 'left', label_first_only = T)
#' doc <- add3c(chart_name, 'middle', T)
#' doc <- add3c(chart_name, 'right', T)
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add3c(chart_name, 'topright')
#' doc <- add3c(chart_name, 'bottomright')
#' doc <- add3c(chart_name, 'left')



### Function
add3c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F) {
  mschart::ph_with_chart_at(
    doc,
    chart = name,
    left = dplyr::case_when(
      position == 'topright' ~ 4.375,
      position == 'righttop' ~ 4.375,
      position == 'bottomright' ~ 4.375,
      position == 'rightbottom' ~ 4.375,
      position == 'left' ~ .0,
      position == 'center' ~ 4.25,
      position == 'right' ~ 8.5

    ),
    top = dplyr::case_when(
      position == 'topright' ~ 2,
      position == 'righttop' ~ 2,
      position == 'bottomright' ~ 4.5,
      position == 'rightbottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'center' ~ 2,
      position == 'right' ~ 2
    ),
    height = dplyr::case_when(
      position == 'topright' ~ 3,
      position == 'righttop' ~ 3,
      position == 'bottomright' ~ 3,
      position == 'rightbottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'center' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = dplyr::case_when(
      position == 'topright' ~ 8.615,
      position == 'righttop' ~ 8.615,
      position == 'bottomright' ~ 8.615,
      position == 'rightbottom' ~ 8.615,
      position == 'left' ~ 4.5,
      position == 'center' ~ 4.5,
      position == 'right' ~ 4.5
    )
  )
  } else{ #label_first_only == T
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'topright' ~ 4.375,
        position == 'righttop' ~ 4.375,
        position == 'bottomright' ~ 4.375,
        position == 'rightbottom' ~ 4.375,
        position == 'left' ~ .0,
        position == 'center' ~ 6.25,
        position == 'right' ~ 9.75

      ),
      top = dplyr::case_when(
        position == 'topright' ~ 2,
        position == 'righttop' ~ 2,
        position == 'bottomright' ~ 4.5,
        position == 'rightbottom' ~ 4.5,
        position == 'left' ~ 2,
        position == 'center' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 3,
        position == 'righttop' ~ 3,
        position == 'bottomright' ~ 3,
        position == 'rightbottom' ~ 3,
        position == 'left' ~ 5.5,
        position == 'center' ~ 5.5,
        position == 'right' ~ 5.5
      ),
      width = dplyr::case_when(
        position == 'topright' ~ 8.615,
        position == 'righttop' ~ 8.615,
        position == 'bottomright' ~ 8.615,
        position == 'rightbottom' ~ 8.615,
        position == 'left' ~ 6.25,
        position == 'center' ~ 3.625,
        position == 'right' ~ 3.625
      )
    )
  }
}

#### Add 4 Charts ####
### Description
#' Add 4 charts to PowerPoint slide
#'
#' This function adds 4 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "left"; "centerleft"; "centerright"; "right". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c(chart_name, 'topright')
#' doc <- add4c(chart_name, 'bottomright')
#' doc <- add4c(chart_name, 'topleft')
#' doc <- add4c(chart_name, 'bottomleft')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c(chart_name, 'left')
#' doc <- add4c(chart_name, 'centerleft')
#' doc <- add4c(chart_name, 'centerright')
#' doc <- add4c(chart_name, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c(chart_name, 'left', label_first_only = T)
#' doc <- add4c(chart_name, 'centerleft', label_first_only = T)
#' doc <- add4c(chart_name, 'centerright', T)
#' doc <- add4c(chart_name, 'right', T)


### Function
add4c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F){
  mschart::ph_with_chart_at(
    doc,
    chart = name,
    left = dplyr::case_when(
      position == 'topright' ~ 6.5,
      position == 'bottomright' ~ 6.5,
      position == 'topleft' ~ .5,
      position == 'bottomleft' ~ .5,
      position == 'left' ~ .0,
      position == 'centerleft' ~ 3.25,
      position == 'centerright' ~ 6.5,
      position == 'right' ~ 9.75
    ),
    top = dplyr::case_when(
      position == 'topright' ~ 2,
      position == 'bottomright' ~ 4.5,
      position == 'topleft' ~ 2,
      position == 'bottomleft' ~ 4.5,
      position == 'left' ~ 2,
      position == 'centerleft' ~ 2,
      position == 'centerright' ~ 2,
      position == 'right' ~ 2
    ),
    height = dplyr::case_when(
      position == 'topright' ~ 3,
      position == 'bottomright' ~ 3,
      position == 'topleft' ~ 3,
      position == 'bottomleft' ~ 3,
      position == 'left' ~ 5.5,
      position == 'centerleft' ~ 5.5,
      position == 'centerright' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = dplyr::case_when(
      position == 'topright' ~ 6,
      position == 'bottomright' ~ 6,
      position == 'topleft' ~ 6,
      position == 'bottomleft' ~ 6,
      position == 'left' ~ 3.5,
      position == 'centerleft' ~ 3.5,
      position == 'centerright' ~ 3.5,
      position == 'right' ~ 3.5
    )
  ) } else{ #label_first == T
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'topright' ~ 8.25,
        position == 'bottomright' ~ 8.25,
        position == 'topleft' ~ 0,
        position == 'bottomleft' ~ 0,
        position == 'left' ~ .0,
        position == 'centerleft' ~ 4.75,
        position == 'centerright' ~ 7.5,
        position == 'right' ~ 10.25
      ),
      top = dplyr::case_when(
        position == 'topright' ~ 2,
        position == 'bottomright' ~ 4.5,
        position == 'topleft' ~ 2,
        position == 'bottomleft' ~ 4.5,
        position == 'left' ~ 2,
        position == 'centerleft' ~ 2,
        position == 'centerright' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 3,
        position == 'bottomright' ~ 3,
        position == 'topleft' ~ 3,
        position == 'bottomleft' ~ 3,
        position == 'left' ~ 5.5,
        position == 'centerleft' ~ 5.5,
        position == 'centerright' ~ 5.5,
        position == 'right' ~ 5.5
      ),
      width = dplyr::case_when(
        position == 'topright' ~ 4.75,
        position == 'bottomright' ~ 4.75,
        position == 'topleft' ~ 8.25,
        position == 'bottomleft' ~ 8.25,
        position == 'left' ~ 5,
        position == 'centerleft' ~ 3,
        position == 'centerright' ~ 3,
        position == 'right' ~ 3
      )
    )
  }
}
#### Add 5 Charts ####
#' Add 5 charts to PowerPoint slide
#'
#' This function adds 5 vertical charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "centerleft"; "center"; centerright"; "right". The chart layout is always 5 tall charts
#' @param label_first_only DEFAULT = F; Set to T if only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add5c(chart_name, 'left')
#' doc <- add5c(chart_name, 'centerleft')
#' doc <- add5c(chart_name, 'center')
#' doc <- add5c(chart_name, 'centerright')
#' doc <- add5c(chart_name, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add5c(chart_name, 'left', label_first_only = T)
#' doc <- add5c(chart_name, 'centerleft', label_first_only = T)
#' doc <- add5c(chart_name, 'center', T)
#' doc <- add5c(chart_name, 'centerright', T)
#' doc <- add5c(chart_name, 'right', T)


### Function
add5c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F){
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'left' ~ -.125,
        position == 'centerleft' ~ 2.5,
        position == 'center' ~ 5.125,
        position == 'centerright' ~ 7.75,
        position == 'right' ~ 10.375
      ),
      top = dplyr::case_when(
        position == 'left' ~ 2,
        position == 'centerleft' ~ 2,
        position == 'center' ~ 2,
        position == 'centerright' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'left' ~ 5.5,
        position == 'centerleft' ~ 5.5,
        position == 'center' ~ 5.5,
        position == 'centerright' ~ 5.5,
        position == 'right' ~ 5.5
      ),
      width = dplyr::case_when(
        position == 'left' ~ 2.875,
        position == 'centerleft' ~ 2.875,
        position == 'center' ~ 2.875,
        position == 'centerright' ~ 2.875,
        position == 'right' ~ 2.875
      )
    ) } else{ #label_first == T
      mschart::ph_with_chart_at(
        doc,
        chart = name,
        left = dplyr::case_when(
          position == 'left' ~ .0,
          position == 'centerleft' ~ 4.4375,
          position == 'center' ~ 6.625,
          position == 'centerright' ~ 8.8125,
          position == 'right' ~ 11
        ),
        top = dplyr::case_when(
          position == 'left' ~ 2,
          position == 'centerleft' ~ 2,
          position == 'center' ~ 2,
          position == 'centerright' ~ 2,
          position == 'right' ~ 2
        ),
        height = dplyr::case_when(
          position == 'left' ~ 5.5,
          position == 'centerleft' ~ 5.5,
          position == 'center' ~ 5.5,
          position == 'centerright' ~ 5.5,
          position == 'right' ~ 5.5
        ),
        width = dplyr::case_when(
          position == 'left' ~ 4.5,
          position == 'centerleft' ~ 2.25,
          position == 'center' ~ 2.25,
          position == 'centerright' ~ 2.25,
          position == 'right' ~ 2.25
        )
      )
    }
}

#### Add 6 Charts ####
### Description
#' Add 6 charts to PowerPoint slide
#'
#' This function adds 6 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "topcenter"; "bottomcenter". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @param label_first_only DEFAULT = F; Set to T when only the first charts on the left has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add6c(chart_name, 'topleft')
#' doc <- add6c(chart_name, 'bottomleft')
#' doc <- add6c(chart_name, 'topcenter')
#' doc <- add6c(chart_name, 'bottomcenter')
#' doc <- add6c(chart_name, 'topright')
#' doc <- add6c(chart_name, 'bottomright')



### Function
add6c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F){
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'topright' ~ 8.5,
        position == 'bottomright' ~ 8.5,
        position == 'topleft' ~ 0,
        position == 'bottomleft' ~ 0,
        position == 'topcenter' ~ 4.25,
        position == 'bottomcenter' ~ 4.25

      ),
      top = dplyr::case_when(
        position == 'topright' ~ 2,
        position == 'bottomright' ~ 4.5,
        position == 'topleft' ~ 2,
        position == 'bottomleft' ~ 4.5,
        position == 'topcenter' ~ 2,
        position == 'bottomcenter' ~ 4.5
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 3,
        position == 'bottomright' ~ 3,
        position == 'topleft' ~ 3,
        position == 'bottomleft' ~ 3,
        position == 'topcenter' ~ 3,
        position == 'bottomcenter' ~ 3
      ),
      width = dplyr::case_when(
        position == 'topright' ~ 4.5,
        position == 'bottomright' ~ 4.5,
        position == 'topleft' ~ 4.5,
        position == 'bottomleft' ~ 4.5,
        position == 'topcenter' ~ 4.5,
        position == 'bottomcenter' ~ 4.5
      )
    ) } else{ #label_first == T
      mschart::ph_with_chart_at(
        doc,
        chart = name,
        left = dplyr::case_when(
          position == 'topright' ~ 9.5,
          position == 'bottomright' ~ 9.5,
          position == 'topleft' ~ 0,
          position == 'bottomleft' ~ 0,
          position == 'topcenter' ~ 5.875,
          position == 'bottomcenter' ~ 5.875

        ),
        top = dplyr::case_when(
          position == 'topright' ~ 2,
          position == 'bottomright' ~ 4.5,
          position == 'topleft' ~ 2,
          position == 'bottomleft' ~ 4.5,
          position == 'topcenter' ~ 2,
          position == 'bottomcenter' ~ 4.5
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 3,
          position == 'bottomright' ~ 3,
          position == 'topleft' ~ 3,
          position == 'bottomleft' ~ 3,
          position == 'topcenter' ~ 3,
          position == 'bottomcenter' ~ 3
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 3.75,
          position == 'bottomright' ~ 3.75,
          position == 'topleft' ~ 6,
          position == 'bottomleft' ~ 6,
          position == 'topcenter' ~ 3.75,
          position == 'bottomcenter' ~ 3.75
          )
      )
    }
}

#### Add 1 Table ####
### Check if flextable

### If NOT flextable
add1_table <- function(
  name,
  add_slide = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  if(add_slide == T) {
    doc <- add1s(slide_name, master_name)
  } else {
    doc <- doc
  }
  officer::ph_with_table_at(
    doc,
    value = name,
    left = left_start,
    top = top_start,
    height = height,
    width = width)
}

### If YES flextable
add1_flextable <- function(
  name,
  add_slide = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = NULL,
  width = NULL
) {
  if(add_slide == T) {
    doc <- add1s(slide_name, master_name)
  } else {
    doc <- doc
  }
  flextable::ph_with_flextable_at(
    doc,
    value = name,
    left = left_start,
    top = top_start
  )
}

### Description
#' Add PowerPoint slide & 1 table
#'
#' This function adds a new PowerPoint slide and fits 1 table onto it. It automatically fits the location to the center of the slide. Beware that if the object passed to the function is not a pre-formated flextable, it automatically assigns a background color to the table and sets the fontsize to 18.
#' @param name The name of the dataframe to be added to a new PowerPoint slide.
#' @param add_slide DEFAULT = T; Automatically adds a blank slide for the chart to be added onto. If F, no new slide is added
#' @param slide_name The name of the type of the PP slide you want added to the PP. DEFAULT: "Findings / 1 chart"
#' @param master_name The name of the PP master layout that the slide_name comes from. DEFAULT: "Office Theme"
#' @keywords chart
#' @export
#' @examples
#' doc <- add1t(my_flextable)
#' OR
#' doc <- add1t(my_table, slide_name = "1_Blank", master = "Custom Design")


### Function
add1t <- function(
  name,
  add_slide = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
    if(class(name)[1] == 'flextable'){
    add1_flextable(name, add_slide, slide_name, master_name, left_start, top_start, height, width)
    } else{
    add1_table(name, add_slide, slide_name, master_name, left_start, top_start, height, width)
    }
}

#### Add 2 Tables ####

### 2 Tables
add2_table <- function(
  name,
  position
) {
  officer::ph_with_table_at(
    doc,
    value = name,
    left = dplyr::case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = dplyr::case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    ),
    height = dplyr::case_when(
      position == 'top' ~ 3,
      position == 'bottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = dplyr::case_when(
      position == 'top' ~ 12,
      position == 'bottom' ~ 12,
      position == 'left' ~ 6,
      position == 'right' ~ 6
    )
  )
}

### 2 Flextables
add2_flextable <- function(
  name,
  position
) {
  flextable::ph_with_flextable_at(
    doc,
    value = name,
    left = dplyr::case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = dplyr::case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    )
  )
}

### Description
#' Add 2 tables to PowerPoint slide
#'
#' This function adds 2 tables to a PowerPoint slide. The tables are automatically added to the last slide of the PP object in R. Beware that if the object passed to the function is not a pre-formated flextable, it automatically assigns a background color to the table and sets the fontsize to 18.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "top"; "bottom"; "left"; "right". The chart layout can either be top-bottom OR left-right.
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2t(my_table, 'left')
#' doc <- add2t(my_table, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2t(my_flextables, 'top')
#' doc <- add2t(my_flextables, 'bottom')

### Function
add2t <- function(
  name,
  position
) {
  if(class(name)[1] == 'flextable'){
    add2_flextable(name, position)
  } else{
    add2_table(name, position)
  }
}


#### ***** GG PLOT ***** ####
#### ggchart_save ####
### Description
#' Quickly save out a chart from ggplot
#'
#' Saves a ggplot chart. All you need to add is the file name
#' @param chartname The name you want to give your file, ex: "brands by age". Do not specify the whole file path. However, you do need to have an object that is the path to your chart folder saved in R as CHART_PATH
#' @param width DEFAULT = 11
#' @param height DEFAULT = 5.5
#' @keywords chart save ggplot
#' @export
#' @examples
#' ggchart_save('An easily saved chart')


ggchart_save <- function(chartname, width = 11, height = 5.5){
  ggplot2::ggsave(
    stringr::str_c(CHART_PATH, '/', chartname, '.png'),
    chart + y2_type,
    width = width,
    height = height,
    bg = 'transparent'
  )
}




#### wc_filter ####
wc_filter <- function(dataset){
  dataset %>%
    filter(
      !label %in% c(
        'AND',
        'THE',
        'I',
        'THAT',
        'TO',
        'A',
        'IT',
        'OF',
        'IS',
        'ARE',
        'ITS',
        'IN',
        'BE',
        'AS',
        'SO',
        'AN',
        'IF',
        'BY',
        'AM',
        'AT',
        'IM',
        'NA',
        'DO',
        'THIS',
        'OR',
        'FOR',
        'YES',
        'NO',
        'NOT',
        'NONE',
        'REALLY',
        'NOTHING',
        'ALL',
        'ME',
        'HE',
        'SHE',
        'MY',
        'WHEN',
        'CAN',
        'ON',
        'DONT',
        'KNOW',
        'IDK',
        'VERY',
        'WITH',
        'FROM',
        'WHO',
        'BUT',
        'WHOM',
        'ALSO',
        'OUR',
        'WOULD',
        'HAVE',
        'WE',
        'BACK',
        'HAS',
        'HAD',
        'YOU',
        'THERE',
        'WAS',
        'HERE',
        'SHOULD',
        'JUST',
        'THEIR',
        'THEM',
        'ABOUT',
        'THEY',
        'WILL'
      )
    )
}
#### openend (wc_prepper) ####
### Description
#' Look at the frequencies of each word in an open end question
#'
#' Breaks down an open ended question on spaces, giving you the frequencies of each word mentioned
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param dataset DEFAULT = responses
#' @keywords openend open end frequencies freqs
#' @export
#' @examples
#' frequencies <- openend(QOPEN_END)

openend <- function(
  variable,
  dataset = responses
) {
  flag <- dplyr::enquo(variable)
  frequencies <- dataset %>%
    select(
      !!flag
    ) %>%
    dplyr::mutate(
      variable = toupper(!!flag)
    ) %>%
    dplyr::select(
      variable
    ) %>%
    tidyr::separate(
      variable,
      into = paste("V", 1:100, sep = "_"),
      sep = ' '
    ) %>%
    tidyr::gather(
      Names,
      Words
    ) %>%
    dplyr::filter(
      Words != ''
    ) %>%
    dplyr::select(
      -Names
    ) %>%
    dplyr::mutate(
      Words = str_replace_all(Words, '\\.COM', ''),
      Words = str_replace_all(Words, "[^[:alnum:]]", ""),
      Words = str_replace_all(Words, 'DO NOT', 'DONT'),
      Words = str_replace_all(Words, 'CAN NOT', 'CANT'),
      Words = str_replace_all(Words, 'CANNOT', 'CANT')
    ) %>%
    y2clerk::freqs(
      Words
    ) %>%
    wc_filter() %>%
    dplyr::filter(
      label != ''
    ) %>%
    dplyr::arrange(
      n %>% desc
    ) %>%
    dplyr::slice(
      1:50
    ) %>%
    dplyr::mutate(
      total =  dataset %>%
        dplyr::filter(
          !is.na(!!flag),
          !!flag != ''
          ) %>%
        dplyr::count() %>%
        as.numeric(),
      result = n / total,
      result = round(result, 2)
    ) %>%
    dplyr::select(
      - total
    )
}

#### wordcloud ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller and lighter in color
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = 'bluepurple'; 4 qualtrics colors as pre-made options: "bluepurple", "lime", "teal", "brightblue". May also specify a vector of 3 scaled colors ranging from lightest to darkest
#' @param dataset DEFAULT = responses
#' @keywords openend open end wordcloud word cloud
#' @export
#' @examples
#' chart <- wordcloud(QOPEN_END)
#' chart <- wordcloud(QOPEN_END, lime)
#' chart <- wordcloud(QOPEN_END, c('#FAEFF2', '#CC6078', '#6D0018'))

wordcloud <- function(
  variable,
  colors = 'bluepurple',
  dataset = responses
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
  wordcloud <- openend(!!flag, dataset)
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
        color = n
      ),
      segment.size = 0
    ) +
    ggplot2::scale_color_gradient2(low = lows, mid = mids, high = highs, midpoint = 0.5, guide = F) +
    ggplot2::scale_size(range = c(1, 12), guide = FALSE) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

#### wordcloud2 ####
### Description
#' Creates a word cloud from an open end question
#'
#' Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param colors DEFAULT = '#474E7E' (bluepurple from Qualtrics template). All words are the same color. Any color may be specified as a hexcode
#' @param dataset DEFAULT = responses
#' @keywords openend open end wordcloud word cloud
#' @export
#' @examples
#' chart <- wordcloud(QOPEN_END)
#' chart <- wordcloud(QOPEN_END, '#FAEFF2')

wordcloud2 <- function(
  variable,
  colors = '#474E7E',
  dataset = responses
) {
  flag <- dplyr::enquo(variable)
  wordcloud <- openend(!!flag, dataset)
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
