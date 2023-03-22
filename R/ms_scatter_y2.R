#### Final ms_scatter_y2 Function ####
#' Create a scatter plot mschart object
#'
#' This function creates an mschart object automatically formatted for a scatter plot. It requires two lists called "text_settings" and "color_settings" and a named vector called "size_settings".
#' @param data DEFAULT = frequencies;The name of the data frame that the mschart pulls from.
#' @param x_var NO DEFAULT
#' @param y_var NO DEFAULT
#' @param group_var DEFAULT = NULL; Can be set to a variable to color code points by that variable.
#' @param label_var DEFAULT = 'label'; This variable will be the text labels that show up on the chart over each point.
#' @param axis_text_size DEFAULT = 14; Font size for variable levels and percentages.
#' @param axis_title_size DEFAULT = 18; Font size for axis_x_label and axis_y_label.
#' @param axis_x_display,axis_y_display DEFAULT = TRUE
#' @param axis_x_label,axis_y_label DEFAULT is automatically set to variable names for x_var and y_var; Title for the x_axis and y_axis
#' @param axis_x_min,axis_y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param axis_x_max,axis_y_max DEFAULT = NULL
#' @param axis_x_rotate,axis_y_rotate DEFAULT = 0; Rotation of axis text. Set to -45 for diagonal, giving more space for text.
#' @param axis_x_rotate_title,axis_y_rotate_title DEFAULT = 0, set y_axis rotation to 360 for horizontal text
#' @param font_family DEFAULT = 'BentonSans Regular' (Qualtrics font). Sets the fonts for axis, legends, and titles. Label font is set within label_color and label_text lists. May specify fonts in quotes, e.g. "Times New Roman"
#' @param label_color DEFAULT = color_settings; A list of color settings for the points Can apply to all points or those of a specific group
#' @param label_position DEFAULT = 't'; Specifies the position of the data label. It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param label_show_values DEFAULT = FALSE; TRUE or FALSE. Show percent labels alongside each point label.
#' @param label_text DEFAULT = text_settings; A list of text settings for the percent labels. This affects font size and color. Specified outside of the function. If a list of one, no need to specify values. Otherwise, they must exactly match the group_var levels. Example: text_settings <- list(fp_text(font.size = 10.5, color = bluepurple))
#' @param legend_pos DEFAULT = 'b' for bottom; All legend positions are 'b', 'n', 't', 'tr', 'l', 'r'.
#' @param legend_text_size DEFAULT = 14
#' @param num_fmt DEFAULT = 'percent'; Can also be set to 'general' for non-percentages. Changes formatting for both the labels and axis
#' @param point_size DEFAULT = size_settings; A named vector of size settings. Must be between 2-72, typically around 10. Can be a vector of one unnamed number if all points are the same size.
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @keywords chart
#' @export
#' @examples
#' ### Grouped
#' color_settings <- list(
#'   '0' = '#082C51',
#'   '1' = '#3E85BD'
#' )
#' size_settings <- c(
#'   '0' = 10,
#'   '1' = 5
#' )
#' text_settings <- list(
#'   '0' = officer::fp_text(color =  '#082C51'  , font.family = 'BentonSans Regular', font.size = 10),
#'   '1' = officer::fp_text(color =  '#3E85BD' , font.family = 'BentonSans Regular', font.size = 10)
#' )
#' frequencies <- mtcars %>%
#'   dplyr::select(mpg, wt, am) %>%
#'   dplyr::mutate(
#'     mpg = mpg/100,
#'     wt = wt/10,
#'     label = rownames(.)
#'   )
#' chart_scatter <- ms_scatter_y2(
#'   x_var = 'mpg',
#'   y_var = 'wt',
#'   group_var = 'am'
#' )
#' print(chart_scatter, preview = TRUE)
#'
#' # Ungrouped
#' color_settings <- list('#082C51')
#' size_settings <- c(10)
#' text_settings <- list('wt' = officer::fp_text(font.family = 'BentonSans Regular'))
#' frequencies <- mtcars %>%
#'   dplyr::select(mpg, wt) %>%
#'   dplyr::mutate(
#'     mpg = mpg/100,
#'     wt = wt/10,
#'     label = rownames(.)
#'   )
#' chart_scatter <- ms_scatter_y2(
#'   x_var = 'mpg',
#'   y_var = 'wt'
#' )
#' print(chart_scatter, preview = TRUE)

ms_scatter_y2 <- function(
  data = frequencies,
  x_var,
  y_var,
  group_var = NULL,
  label_var = 'label',
  axis_text_size = 14,
  axis_title_size = 18,
  axis_x_display = TRUE,
  axis_x_label = 'x_var',
  axis_x_min = 0,
  axis_x_max = NULL,
  axis_x_rotate = 0,
  axis_x_rotate_title = 0,
  axis_y_display = TRUE,
  axis_y_label = 'y_var',
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_rotate = 0,
  axis_y_rotate_title = 0,
  font_family = 'BentonSans Regular',
  label_color = color_settings,
  label_position = 't',
  label_show_values = FALSE,
  label_text = text_settings,
  legend_pos = c('b', 'n', 't', 'tr', 'l', 'r'),
  legend_text_size = 14,
  num_fmt = c('percent', 'general'),
  point_size = size_settings,
  title_label = '',
  title_size = 18
){

  ### Check for special symbols
  freqs_list <- split(data, seq(nrow(data))) # turn data frame into a list
  symbols_sum <- purrr::map_df(freqs_list, ~stringr::str_detect(.x, "<|&")) %>% # test if any cells contain special symbols
    dplyr::mutate_all(~as.numeric(.)) %>% # convert table into numerics
    sum(na.rm = TRUE) # sum all cells to count the number of special symbols
  if(symbols_sum > 0){
    stop('mschart objects cannot contain the special symbols "&" or "<". Please remove those symbols from your data frame')
  }

  ### Flags
  legend_pos <- rlang::arg_match(legend_pos)
  num_fmt <- rlang::arg_match(num_fmt)
  axis_num_fmt <- dplyr::case_when(
    num_fmt == 'percent' ~ '0%',
    num_fmt == 'general' ~ 'general'
  )
  label_num_fmt <- dplyr::case_when(
    num_fmt == 'percent' ~ '0%',
    num_fmt == 'general' ~ 'general'
  )
  find_xy_means(data, x_var, y_var)

  ### Update defaults
  legend_pos <- dplyr::case_when(
    is.null(group_var) ~ 'n',
    TRUE ~ legend_pos
  )
  axis_y_label <- dplyr::case_when(
    axis_y_label == 'y_var' ~ y_var,
    TRUE ~ axis_y_label
  )
  axis_x_label <- dplyr::case_when(
    axis_x_label == 'x_var' ~ x_var,
    TRUE ~ axis_x_label
  )

  ### Chart
  mschart::ms_scatterchart(
    data = data,
    x = x_var,
    y = y_var,
    group = group_var,
    labels = label_var
  ) %>%
    mschart::chart_data_labels(
      show_val = label_show_values,
      #show_percent = FALSE,
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
    mschart::chart_data_size(
      values = point_size
    ) %>%
    mschart::chart_ax_y(
      num_fmt = axis_num_fmt,
      limit_min = axis_y_min,
      limit_max = axis_y_max
    ) %>%
    mschart::chart_ax_x(
      num_fmt = axis_num_fmt,
      limit_min = axis_x_min,
      limit_max = axis_x_max
    ) %>%
    mschart::chart_labels(
      xlab = axis_x_label,
      ylab = axis_y_label,
      title = title_label
    ) %>%
    mschart::chart_theme(
      legend_position = legend_pos,
      main_title = officer::fp_text(font.size = title_size, font.family = font_family),
      axis_text_x = officer::fp_text(font.size = axis_text_size, font.family = font_family),
      axis_text_y = officer::fp_text(font.size = axis_text_size, font.family = font_family),
      axis_title_x = officer::fp_text(font.size = axis_title_size, font.family = font_family),
      axis_title_y = officer::fp_text(font.size = axis_title_size, font.family = font_family),
      legend_text = officer::fp_text(font.size = legend_text_size, font.family = font_family),
      grid_major_line_x = officer::fp_border(width = 0),
      grid_major_line_y = officer::fp_border(width = 0),
      title_y_rot = axis_y_rotate_title,
      title_x_rot = axis_x_rotate_title
    ) %>%
    mschart::chart_ax_x(
      display = axis_x_display
    )  %>%
    mschart::chart_ax_y(
      display = axis_y_display
    ) %>%
    mschart::chart_ax_x(
      rotation = axis_x_rotate
    ) %>%
    mschart::chart_ax_y(
      rotation = axis_y_rotate
    )
}


# Private Functions -------------------------------------------------------

# Finding x & y means

find_xy_means <- function(
  data,
  x_var,
  y_var
) {

  # Convert variable strings to symbols
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)

  # Get the means
  mean_x <- data %>%
    y2clerk::freqs(
      {{ x_sym }},
      nas = FALSE,
      stat = 'mean'
    ) %>%
    dplyr::select('result') %>%
    as.character()
  x_statement <- stringr::str_c('x_var mean (', x_var, '): ', mean_x)

  mean_y <- data %>%
    y2clerk::freqs(
      {{ y_sym }},
      nas = FALSE,
      stat = 'mean'
    ) %>%
    dplyr::select('result') %>%
    as.character()
  y_statement <- stringr::str_c('y_var mean (', y_var, '): ', mean_y)


  # Print out the means
  cat(
    x_statement,
    y_statement,
    sep = '\n'
  )
}

