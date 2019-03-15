#### Bar Single ####
### Description
#' Create an ugrouped mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data The name of the data frame that the mscharts pulls from.
#' @param x_var When using the freqs function, will typically be label (is by default).
#' @param y_var When using the freqs function, will typically be result (is by default).
#' @param group_var Is null by default. If you want the bars to be different colors, set group_var to the same variable as x_var. Then set overlap to 100.
#' @param label_text A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings <- list(fp_text(font.size = 10.5, color = bluepurple))
#' @param label_color A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings <- list(bluepurple)
#' @param label_show_values TRUE or FALSE. Show percent labels for each value.
#' @param label_position Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label Title for the x_axis. Blank by default.
#' @param axis_y_label Title for the y_axis. Blank by default.
#' @param axis_y_min Set to 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max No maximum specified by default.
#' @param axis_text_size Font size for variable levels and percentages.
#' @param axis_title_size Font size for axis_x_label and axis_y_label.
#' @param title_label Add the question wording from the survey in "" as the title of the chart.
#' @param rotate Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlapping Set to -50 by default. This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param direction Two options: "vertical" (default) OR "horizontal"
#' @param legend_pos Set to 'n' for none. Other legend positions are 'b', 'tr', 'l', 'r', 't'.
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
  overlapping = -50,
  direction = 'vertical',
  legend_text_size = 16,
  legend_pos = 'n'
) {
  ms_barchart(
    data,
    x = x_var,
    y = y_var,
    group = group_var
  ) %>%
    chart_settings(
      dir = direction,
      grouping = grouping,
      overlap = overlapping
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
    chart_ax_x(
      rotation = rotate
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
    chart_ax_x(
      display = axis_x_display
    )  %>%
    chart_ax_y(
      display = axis_y_display
    )
}


#### Bar Grouped ####
### Description
#' Create a groupedbar mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data The name of the data frame that the mscharts pulls from.
#' @param x_var When using the freqs function, will typically be label (is by default).
#' @param y_var When using the freqs function, will typically be result (is by default).
#' @param group_var All levels of the group_var must be present or the chart may break. To do this, save the variable as_factor() before running freqs. Also remember that label_text and label_color must exactly match all the levels of the group_var or the function will break.
#' @param label_text A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings_grouped <- list('Name of Group 1' = fp_text(font.size = 16, color = lime),'Name of Group 2' = fp_text(font.size = 16, color = brightblue))
#' @param label_color A list of color settings for the bars. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings_grouped <- list('Name of Group 1' = lime,'Name of Group 2' = brightblue)
#' @param label_show_values TRUE or FALSE. Show percent labels for each value.
#' @param label_position Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label Title for the x_axis. Blank by default.
#' @param axis_y_label Title for the y_axis. Blank by default.
#' @param axis_y_min Set to 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max No maximum specified by default.
#' @param axis_text_size Font size for variable levels and percentages.
#' @param axis_title_size Font size for axis_x_label and axis_y_label.
#' @param title_label Add the question wording from the survey in "" as the title of the chart.
#' @param rotate Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlapping Set to -50 by default. This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param direction Two options: "vertical" (default) OR "horizontal"
#' @param legend_pos Set to 't' for top. Other legend positions are 'b', 'tr', 'l', 'r', and 'n' for none.
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
  overlapping = -50,
  direction = 'vertical',
  legend_text_size = 16,
  legend_pos = 't'
) {
  ms_barchart(
    data,
    x = x_var,
    y = y_var,
    group = group_var
  ) %>%
    chart_settings(
      dir = direction,
      grouping = grouping,
      overlap = overlapping
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
    chart_ax_x(
      rotation = rotate
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
    chart_ax_x(
      display = axis_x_display
    )  %>%
    chart_ax_y(
      display = axis_y_display
    )
}

#### Bar Stacked ####
### Description
#' Create a groupedbar mschart object
#'
#' This function creates a mschart object automatically formatted for a single variable (including multiple select). It requires two lists called "text_settings" and "color_settings" by default that specify the colors desired for the chart.
#' @param data The name of the data frame that the mscharts pulls from.
#' @param x_var For a single stacked bar, use
#' @param y_var When using the freqs function, will typically be result (is by default).
#' @param group_var When using the freqs function, will typically be label (is by default). All levels of the group_var must be present or the chart may break. To do this, save the variable as_factor() before running freqs. Also remember that label_text and label_color must exactly match all the levels of the group_var or the function will break.
#' @param label_text A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings_grouped <- list('Name of Group 1' = fp_text(font.size = 16, color = lime),'Name of Group 2' = fp_text(font.size = 16, color = brightblue))
#' @param label_color A list of color settings for the levels within each stacked bar. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: color_settings_grouped <- list('Name of Group 1' = lime,'Name of Group 2' = brightblue)
#' @param label_show_values TRUE or FALSE. Show percent labels for each value.
#' @param label_position Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_num_fmt Number formatting specifies number format properties which indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param axis_num_fmt Unlike label_num_fmt, the default for percentages is "0\%\%".
#' @param axis_x_label Title for the x_axis. Blank by default.
#' @param axis_y_label Title for the y_axis. Blank by default.
#' @param axis_y_min Set to 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_y_max Set to 1 by default to allow percent totals to add to 100%.
#' @param axis_x_text_color Default: 'black'. Specify 'transparent' for single bar to remove pointless label.
#' @param axis_text_size Font size for variable levels and percentages.
#' @param axis_title_size Font size for axis_x_label and axis_y_label.
#' @param title_label Add the question wording from the survey in "" as the title of the chart.
#' @param rotate Rotation of x_axis text. Set to -45 for diagonal, giving more space for text.
#' @param grouping grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlapping Set to -50 by default. This leaves 50\% extra space between variable levels. Set to 100 when coloring bars different colors.
#' @param direction Two options: "horizontal" (default) OR "vertical"
#' @param legend_pos Set to 't' for top. Other legend positions are 'b', 'tr', 'l', 'r', and 'n' for none.
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
  overlapping = 100,
  direction = 'horizontal',
  legend_text_size = 16,
  legend_pos = 't',
){
  ms_barchart(
    data,
    x = x_var,
    y = y_var,
    group = group_var
  ) %>%
    chart_settings(
      dir = direction,
      grouping = grouping,
      overlap = overlapping
    ) %>%
    chart_data_labels(
      show_val = label_show_values,
      num_fmt = label_num_fmt,
      show_percent = label_show_percent
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
    chart_ax_x(
      rotation = rotate
    ) %>%
    chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    chart_theme(
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
    chart_ax_x(
      display = axis_x_display
    )  %>%
    chart_ax_y(
      display = axis_y_display
    )
}

#### Add 1 Chart ####
### Description
#' Add PowerPoint slide & 1 chart
#'
#' This function adds a new PowerPoint slide and fits 1 chart onto it. It automatically fits the location to the center of the slide.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param slide_name The name of the type of the PP slide you want added to the PP. DEFAULT: "Findings / 1 chart"
#' @param master_name The name of the PP master layout that the slide_name comes from. DEFAULT: "Office Theme"
#' @keywords chart
#' @export
#' @examples
#' doc <- add1c(my_ms_chart)
#' OR
#' doc <- add1c(my_ms_chart, slide_name = "1_Blank", master = "Custom Design")

### Function
add1c <- function(
  name,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  doc <- add_slide(
    doc,
    layout = slide_name,
    master = master_name)
  ph_with_chart_at(
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
#' @keywords chart
#' @export
#' @examples
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'left')
#' doc <- add2c(chart_name, 'right')
#' OR
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'top')
#' doc <- add2c(chart_name, 'bottom')

### Function
add2c <- function(
  name,
  position
) {
  ph_with_chart_at(
    doc,
    chart = name,
    left = case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    ),
    height = case_when(
      position == 'top' ~ 3,
      position == 'bottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = case_when(
      position == 'top' ~ 12,
      position == 'bottom' ~ 12,
      position == 'left' ~ 6,
      position == 'right' ~ 6
    )
  )
}

#### Add 3 Charts ####
### Description
#' Add 3 charts to PowerPoint slide
#'
#' This function adds 3 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "center"; "right"; "bottomright"; "topright". The chart layout can either be left-center-right OR left-topright-bottomright.
#' @keywords chart
#' @export
#' @examples
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add3c(chart_name, 'left')
#' doc <- add3c(chart_name, 'middle')
#' doc <- add3c(chart_name, 'right')
#' OR
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add3c(chart_name, 'topright')
#' doc <- add3c(chart_name, 'bottomright')
#' doc <- add3c(chart_name, 'left')


### Function
add3c <- function(
  name,
  position
) {
  ph_with_chart_at(
    doc,
    chart = name,
    left = case_when(
      position == 'topright' ~ 4.375,
      position == 'righttop' ~ 4.375,
      position == 'bottomright' ~ 4.375,
      position == 'rightbottom' ~ 4.375,
      position == 'left' ~ .0,
      position == 'center' ~ 4.25,
      position == 'middle' ~ 4.25,
      position == 'right' ~ 8.5

    ),
    top = case_when(
      position == 'topright' ~ 2,
      position == 'righttop' ~ 2,
      position == 'bottomright' ~ 4.5,
      position == 'rightbottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'center' ~ 2,
      position == 'middle' ~ 2,
      position == 'right' ~ 2
    ),
    height = case_when(
      position == 'topright' ~ 3,
      position == 'righttop' ~ 3,
      position == 'bottomright' ~ 3,
      position == 'rightbottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'center' ~ 5.5,
      position == 'middle' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = case_when(
      position == 'topright' ~ 8.615,
      position == 'righttop' ~ 8.615,
      position == 'bottomright' ~ 8.615,
      position == 'rightbottom' ~ 8.615,
      position == 'left' ~ 4.5,
      position == 'center' ~ 4.5,
      position == 'middle' ~ 4.5,
      position == 'right' ~ 4.5
    )
  )
}

#### Add 4 Charts ####
### Description
#' Add 4 charts to PowerPoint slide
#'
#' This function adds 4 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "left"; "centerleft"; "centerright"; "right". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @keywords chart
#' @export
#' @examples
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c(chart_name, 'topright')
#' doc <- add4c(chart_name, 'bottomright')
#' doc <- add4c(chart_name, 'topleft')
#' doc <- add4c(chart_name, 'bottomleft')
#' OR
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c(chart_name, 'left')
#' doc <- add4c(chart_name, 'centerleft')
#' doc <- add4c(chart_name, 'centerright')
#' doc <- add4c(chart_name, 'right')


### Function
add4c <- function(
  name,
  position,
  label_first = F
) {
  if(label_first == F){
  ph_with_chart_at(
    doc,
    chart = name,
    left = case_when(
      position == 'topright' ~ 6.5,
      position == 'righttop' ~ 6.5,
      position == 'bottomright' ~ 6.5,
      position == 'rightbottom' ~ 6.5,
      position == 'topleft' ~ .5,
      position == 'lefttop' ~ .5,
      position == 'bottomleft' ~ .5,
      position == 'leftbottom' ~ .5,
      position == 'left' ~ .0,
      position == 'centerleft' ~ 3.25,
      position == 'centerright' ~ 6.5,
      position == 'right' ~ 9.75
    ),
    top = case_when(
      position == 'topright' ~ 2,
      position == 'righttop' ~ 2,
      position == 'bottomright' ~ 4.5,
      position == 'rightbottom' ~ 4.5,
      position == 'topleft' ~ 2,
      position == 'lefttop' ~ 2,
      position == 'bottomleft' ~ 4.5,
      position == 'leftbottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'centerleft' ~ 2,
      position == 'centerright' ~ 2,
      position == 'right' ~ 2
    ),
    height = case_when(
      position == 'topright' ~ 3,
      position == 'righttop' ~ 3,
      position == 'bottomright' ~ 3,
      position == 'rightbottom' ~ 3,
      position == 'topleft' ~ 3,
      position == 'lefttop' ~ 3,
      position == 'bottomleft' ~ 3,
      position == 'leftbottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'centerleft' ~ 5.5,
      position == 'centerright' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = case_when(
      position == 'topright' ~ 6,
      position == 'righttop' ~ 6,
      position == 'bottomright' ~ 6,
      position == 'rightbottom' ~ 6,
      position == 'topleft' ~ 6,
      position == 'lefttop' ~ 6,
      position == 'bottomleft' ~ 6,
      position == 'leftbottom' ~ 6,
      position == 'left' ~ 3.5,
      position == 'centerleft' ~ 3.5,
      position == 'centerright' ~ 3.5,
      position == 'right' ~ 3.5
    )
  ) } else{ #label_first == T
    ph_with_chart_at(
      doc,
      chart = name,
      left = case_when(
        position == 'topright' ~ 6.5,
        position == 'righttop' ~ 6.5,
        position == 'bottomright' ~ 6.5,
        position == 'rightbottom' ~ 6.5,
        position == 'topleft' ~ .5,
        position == 'lefttop' ~ .5,
        position == 'bottomleft' ~ .5,
        position == 'leftbottom' ~ .5,
        position == 'left' ~ .0,
        position == 'centerleft' ~ 4.75,
        position == 'centerright' ~ 7.5,
        position == 'right' ~ 10.25
      ),
      top = case_when(
        position == 'topright' ~ 2,
        position == 'righttop' ~ 2,
        position == 'bottomright' ~ 4.5,
        position == 'rightbottom' ~ 4.5,
        position == 'topleft' ~ 2,
        position == 'lefttop' ~ 2,
        position == 'bottomleft' ~ 4.5,
        position == 'leftbottom' ~ 4.5,
        position == 'left' ~ 2,
        position == 'centerleft' ~ 2,
        position == 'centerright' ~ 2,
        position == 'right' ~ 2
      ),
      height = case_when(
        position == 'topright' ~ 3,
        position == 'righttop' ~ 3,
        position == 'bottomright' ~ 3,
        position == 'rightbottom' ~ 3,
        position == 'topleft' ~ 3,
        position == 'lefttop' ~ 3,
        position == 'bottomleft' ~ 3,
        position == 'leftbottom' ~ 3,
        position == 'left' ~ 5.5,
        position == 'centerleft' ~ 5.5,
        position == 'centerright' ~ 5.5,
        position == 'right' ~ 5.5
      ),
      width = case_when(
        position == 'topright' ~ 6,
        position == 'righttop' ~ 6,
        position == 'bottomright' ~ 6,
        position == 'rightbottom' ~ 6,
        position == 'topleft' ~ 6,
        position == 'lefttop' ~ 6,
        position == 'bottomleft' ~ 6,
        position == 'leftbottom' ~ 6,
        position == 'left' ~ 5,
        position == 'centerleft' ~ 3,
        position == 'centerright' ~ 3,
        position == 'right' ~ 3
      )
    )
  }
}
#### Add 1 Table ####
### Check if flextable

### If NOT flextable
add1_table <- function(
  name,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  doc <- add_slide(
    doc,
    layout = slide_name,
    master = master_name)
  ph_with_table_at(
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
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = NULL,
  width = NULL
) {
  doc <- add_slide(
    doc,
    layout = slide_name,
    master = master_name)
  ph_with_flextable_at(
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
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  ifelse(
    class(name) == 'flextable',
    add1_flextable(name, slide_name, master_name, left_start, top_start, height, width),
    add1_table(name, slide_name, master_name, left_start, top_start, height, width)
  )
}

#### Add 2 Tables ####

### 2 Tables
add2_table <- function(
  name,
  position
) {
  ph_with_table_at(
    doc,
    value = name,
    left = case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.5,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    ),
    height = case_when(
      position == 'top' ~ 3,
      position == 'bottom' ~ 3,
      position == 'left' ~ 5.5,
      position == 'right' ~ 5.5
    ),
    width = case_when(
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
  ph_with_chart_at(
    doc,
    value = name,
    left = case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = case_when(
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
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2t(my_table, 'left')
#' doc <- add2t(my_table, 'right')
#' OR
#' doc <- add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2t(my_flextables, 'top')
#' doc <- add2t(my_flextables, 'bottom')

### Function
add2t <- function(
  name,
  position
) {
  ifelse(
    class(name) == 'flextable',
    add2_flextable(name, position),
    add2_table(name, position)
  )
}

