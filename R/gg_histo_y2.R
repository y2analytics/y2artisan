#### gg_grouped_y2 ####
### Description
#' Create a ggplot2 histogram
#'
#' This function creates a histogram of a given variable with a dotted vertical line at the mean
#' @param data NO DEFAULT; a data frame containing the variable you want to histogram
#' @param x_var NO DEFAULT; the variable name for which you want to create a histogram
#' @param fills NO DEFAULT; the fill color for the histogram
#' @param axis_text_size DEFAULT = 16; Font size for scale points along the axis
#' @param axis_title_size DEFAULT = 18; Font size for x_label and y_label
#' @param bins DEFAULT = 30; The number of bins. Same as the ggplot2 function, geom_histogram
#' @param binwidth DEFAULT = NULL; The number of units in the x_var that fit in a bin. Overrides the bins argument
#' @param x_limits DEFAULT = 'no limits'; The 'no limits' default allows the histogram to capture all values for the variable. A secondary option is '95 trim' which will set the limits of the histogram to within 2 standard deviations of the mean, or all values between the 5th and 95th percentiles. Alternatively, you can set your own limits using c(my_min, my_max)
#' @param x_label DEFAULT = ''; Title for the x_axis
#' @param x_label DEFAULT = 'Respondents'; Title for the x_axis (the y axis is set to show the number of respondents in a given bin)
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' chart <- responses %>% gg_histo_y2(
#'   qrating,
#'   'purple',
#'   x_limits = '95 trim'
#' )

gg_histo_y2 <- function(
  data,
  x_var,
  fills,
  axis_text_size = 16,
  axis_title_size = 18,
  bins = NULL,
  binwidth = NULL,
  color_mean_line = '#474747',
  x_limits = 'no limits',
  x_label = '',
  y_label = 'Respondents'
){
# Variable prep
  var_flag <- enquo(x_var)

  mean_overall <- data %>%
    freqs(!!var_flag, nas = F, stat = 'mean') %>%
    select(result) %>% as.numeric()

  var_95 <- data %>% freqs(!!var_flag, nas = F, stat = 'quantile', pr = 95) %>% select(result) %>% as.numeric()
  var_05 <- data %>% freqs(!!var_flag, nas = F, stat = 'quantile', pr = 5) %>% select(result) %>% as.numeric()
  var_min <- data %>% freqs(!!var_flag, nas = F, stat = 'min') %>% select(result) %>% as.numeric()
  var_max <- data %>% freqs(!!var_flag, nas = F, stat = 'max') %>% select(result) %>% as.numeric()

  x_limits = if(x_limits == 'no limits'){
    c(var_min, var_max)
  } else if(x_limits == '95 trim'){
    c(var_05, var_95)
  } else{
    x_limits
  }


# Actual Chart
  chart <- ggplot() +
    geom_histogram(
      data = data ,
      aes(x = !!var_flag),
      fill = fills,
      alpha = 0.8,
      color = NA,
      binwidth = binwidth,
      bins = bins
    ) +
    geom_vline(
      xintercept = mean_overall,
      linetype = 'dashed',
      color = color_mean_line
    ) +
    scale_y_continuous(
    ) +
    scale_x_continuous(
      limits = x_limits
    ) +
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size)
    )
}
