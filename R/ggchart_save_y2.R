#### ggchart_save_y2 ####
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
#' # To save your chart, you will need CHART_PATH, a path to your folder destination
#'
#' CHART_PATH <- '~/Desktop/'
#'
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#' chart <- gg_single_y2(font_family = "sans")
#'
#' ggchart_save_y2('test')


ggchart_save_y2 <- function(
  chartname,
  width = 11,
  height = 5.5
  ){
  ggplot2::ggsave(
    stringr::str_c(CHART_PATH, '/', chartname, '.png'),
    chart,
    width = width,
    height = height,
    bg = 'transparent'
  )
}




