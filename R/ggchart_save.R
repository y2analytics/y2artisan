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




