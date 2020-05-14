#### gg_stacked_y2 ####
### Description
#' Create a grouped ggplot object
#'
#' This function creates a ggplot2 object automatically formatted for a stacked variable with inherent ordering.
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = group_var; When doing a single stacked bar, set x_var to variable or any variable that has a only one level
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param label_var DEFAULT = percent_label; When using the order_label function, this variable will be created for you.
#' @param color_var DEFAULT = label. Note: the color variable CANNOT be numeric.
#' @param axis_display DEFAULT = TRUE; for a single stacked bar, set to FALSE to remove axis labels
#' @param axis_text_size DEFAULT = 12; Font size for variable levels and axis percentages.
#' @param axis_title_size DEFAULT = 14; Font size for x_label and y_label.
#' @param bar_width DEFAULT = .75, with a bar_width of 1 meaning each bars touches the ones next to it
#' @param erase_labels DEFAULT = .01; all percent labels less than or equal to erase_labels will be erased to avoid clutter and overlapping labels. This argument pulls from the value in the result column of the dataframe being used
#' @param fills NO DEFAULT; requires a vector of colors for all levels of the color_var
#' @param font_family DEFAULT = 'flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param colors DEFAULT is white ('#ffffff') for the text of all percent labels; You may also 1) Specify 1 color, and this color will be applied to all color_var levels or 2) Specify a vector of colors for each individual level of the color_var
#' @param label_length DEFAULT = 45 for horizontal charts and 15 for vertical charts. This determines how many characters an x-axis label can be before R inserts a line break.
#' @param label_size DEFAULT = 8. Adjusts the size of the percent labels within each bar.
#' @param legend_nrow DEFAULT = NULL; Change to a numeric to specify the number of rows for the legend
#' @param legend_pos DEFAULT = 'top'
#' @param legend_rev DEFAULT = FALSE
#' @param legend_text_size DEFAULT = 8
#' @param legend_title_size DEFAULT = 8
#' @param legend_title DEFAULT = '', If you put in a title, the legend will default to 'top' unless otherwise specified
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param y_label DEFAULT = ''; Title for the y_axis
#' @param y_min DEFAULT = 0. Change to a negative value such as -.2 to add space for extra text/graphic between the axis text and the stacked bars
#' @keywords chart ggplot bar stacked
#' @export
#' @examples
#' frequencies <- tibble(
#'   label = rep(c('Promoter', 'Passive', 'Detractor'), 3),
#'   result = c(.33, .33, .34, .20, .30, .50, .25, .50, .25),
#'   value = rep(c(1, 2, 3), 3),
#'   group_var = c(rep('Group A', 3), rep('Group B', 3), rep('Group C', 3))
#' ) %>% order_label(
#'   group_var = group_var,
#'   stacked = 'gg'
#' )
#'
#' chart <- gg_stacked_y2(
#'   fills = c('red', 'yellow', 'green')
#' )
#'
#'
#' # For a single stacked bar:
#' frequencies <- tibble(
#'  label = c('Promoter', 'Passive', 'Detractor'),
#'  result = c(.33, .33, .34),
#'  value = c(1, 2, 3),
#'  variable = rep('QNPS', 3)
#' ) %>% order_label(stacked = 'gg')
#'
#' chart <- gg_stacked_y2(
#'    x_var = variable,
#'    axis_display = F,
#'    fills = c('red', 'yellow', 'green')
#' )

gg_stacked_y2 <- function(
  data = frequencies,
  x_var = group_var,
  y_var = result,
  label_var = percent_label,
  color_var = label,
  axis_display = T,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  colors = '0',
  direction = c("horizontal", 'vertical'),
  erase_labels = .01,
  fills, #only argument with no default
  font_family = 'flama',
  label_length = 45,
  label_size = 8,
  legend_nrow = NULL,
  legend_pos = 'top',
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = '',
  title_label = '',
  title_size = 14,
  x_label = '',
  y_label = '',
  y_min = 0
) {

### Check fonts
if(
  font_family == 'flama' &
  (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
){
  stop("The font you specified in the 'font_family' argument does not exist in your R session")
}



### Flags
  x_flag <- dplyr::enquo(x_var) #probs group_var
  y_flag <- dplyr::enquo(y_var)
  color_flag <- dplyr::enquo(color_var) #probs label
  label_flag <- dplyr::enquo(label_var)
  direction <- rlang::arg_match(direction)



### Set defaults
  max_lab <- data %>% dplyr::select(!!color_flag) %>% dplyr::distinct() %>% purrr::as_vector() %>% length()
  colors = dplyr::case_when(
    colors == '0' ~ c(rep('#ffffff', max_lab)),
    length(colors) == 1 ~ c(rep('#ffffff', max_lab)),
    T ~ colors
  )
  label_length <- dplyr::case_when(
    label_length != 45 ~ label_length,
    T ~ label_length
  )
  legend_rev <- ifelse(legend_rev == FALSE, TRUE, FALSE)



### Conditional chunks
cond_axis_display <-  if(axis_display == F){
    ggplot2::theme(
      axis.text = element_blank()
    )
  } else{NULL}

cond_direction <- if(direction == 'horizontal'){
  ggplot2::coord_flip()
} else{NULL}



### Chart
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!x_flag,
      y = !!y_flag,
      fill = !!color_flag
    )
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(
        fill = !!color_flag
      ),
      stat = 'identity',
      position = 'fill',
      width = bar_width
    ) +
    ggplot2::geom_text(
      data = data %>% dplyr::mutate(
        percent_label = !!label_flag,
        percent_label = dplyr::case_when(
          result <= erase_labels ~ '',
          T ~ percent_label
        )
        ),
      ggplot2::aes(
        label = !!label_flag,
        color = !!color_flag
      ),
      family = font_family,
      size = label_size,
      position = ggplot2::position_fill(vjust = 0.5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(
        reverse = legend_rev,
        nrow = legend_nrow
        ),
      values = fills
    ) +
    ggplot2::scale_color_manual(
      guide = FALSE,
      values = colors
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
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      legend.position = legend_pos,
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = title_size),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, 1),
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
    cond_direction +
    cond_axis_display
}


