#### gg_grouped_y2 ####
### Description
#' Create a ggplot2 histogram
#'
#' This function creates a ggplot2 histogram object of a given variable with a dotted vertical line at the mean
#' @param data NO DEFAULT; a data frame containing the variable you want to histogram
#' @param x_var NO DEFAULT; the variable name for which you want to create a histogram
#' @param fills NO DEFAULT; the fill color for the histogram
#' @param axis_text_size DEFAULT = 16; Font size for scale points along the axis
#' @param axis_title_size DEFAULT = 18; Font size for x_label and y_label
#' @param bins DEFAULT = 30; The number of bins. Same as the ggplot2 function, geom_histogram
#' @param binwidth DEFAULT = NULL; The number of units in the x_var that fit in a bin. Overrides the bins argument
#' @param color_mean_line DEFAULT = '#474747', a gray/black color
#' @param font_family DEFAULT = 'flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param mean_line DEFAULT = "mean", must be one of c("mean", "median", "none")
#' @param quadrant_lines DEFAULT = FALSE. Set to TRUE to display dotted lines on the 25th and 75th percentiles
#' @param weight_var DEFAULT = NULL; set to your weights variable if working with weighted data
#' @param x_limits DEFAULT = 'no limits'; The 'no limits' default allows the histogram to capture all values for the variable. A secondary option is '95 trim' which will set the limits of the histogram to within 2 standard deviations of the mean, or all values between the 5th and 95th percentiles. Alternatively, you can set your own limits using c(my_min, my_max)
#' @param x_label DEFAULT = ''; Title for the x_axis
#' @param y_label DEFAULT = 'Respondents'; Title for the x_axis (the y axis is set to show the number of respondents in a given bin)
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' chart <- iris %>% gg_histo_y2(
#'   Petal.Width,
#'   'purple',
#'   binwidth = .25,
#'   font_family = 'sans'
#' )

gg_histo_y2 <- function(
  data,
  x_var,
  fills = '#474E7E',
  axis_text_size = 16,
  axis_title_size = 18,
  bins = NULL,
  binwidth = NULL,
  color_mean_line = '#474747',
  font_family = 'flama',
  mean_line = c("mean", "median", "none"),
  quadrant_lines = FALSE,
  weight_var = NULL,
  x_limits = 'no limits',
  x_label = '',
  y_label = 'Respondents'
){

### Check fonts
if(
  font_family == 'flama' &
  (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
){
  stop("The font you specified in the 'font_family' argument does not exist in your R session")
}



### Flags
  var_flag <- dplyr::enquo(x_var)
  wt_flag <- dplyr::enquo(weight_var)
  mean_line <- rlang::arg_match(mean_line)



### Set defaults
  mean_overall <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'mean',
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  median_overall <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'median',
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  line25 <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'quantile',
      pr = 25,
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  line75 <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'quantile',
      pr = 75,
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()
  quadrants <- c(line25, line75)

  var_95 <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'quantile',
      pr = 95,
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  var_05 <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'quantile',
      pr = 5,
      wt = !!wt_flag
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  var_min <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'min'
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  var_max <- data %>%
    y2clerk::freqs(
      !!var_flag,
      nas = FALSE,
      stat = 'max'
      ) %>%
    dplyr::select(.data$result) %>%
    as.numeric()

  x_limits = if(x_limits == 'no limits'){
    c(var_min, var_max)
  } else if(x_limits == '95 trim'){
    c(var_05, var_95)
  } else{
    x_limits
  }



### Conditional chunks
  # mean
  cond_mean <- if(mean_line == "mean"){
    ggplot2::geom_vline(
      xintercept = mean_overall,
      linetype = 'dashed',
      color = color_mean_line
    )
  } else if (mean_line == "median"){
    ggplot2::geom_vline(
      xintercept = median_overall,
      linetype = 'dashed',
      color = color_mean_line
    )
  } else{
    NULL
  }
  # quadrants
  cond_quadrants <- if(quadrant_lines == TRUE){
    ggplot2::geom_vline(
      xintercept = c(quadrants),
      linetype = 'dashed',
      color = color_mean_line
    )
  } else{
    NULL
  }



### Chart
  chart <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = data ,
      ggplot2::aes(
        x = !!var_flag,
        weight = !!wt_flag
        ),
      fill = fills,
      alpha = 0.8,
      color = NA,
      binwidth = binwidth,
      bins = bins
    ) +
    cond_mean +
    cond_quadrants +
    ggplot2::scale_y_continuous(
    ) +
    ggplot2::scale_x_continuous(
      limits = x_limits
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      text = ggplot2::element_text(family = font_family)
    )

  return(chart)
}
