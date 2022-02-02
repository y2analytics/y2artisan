# Public function ---------------------------------------------------------
### ms_print_y2

#' Print an ms_chart object for your viewing pleasure
#'
#' Provide a chart object created using the y2artisan suite of ms_charts function to see a preview of the chart.
#'
#' @keywords preview print ms charts
#' @param chart_name DEFAULT = chart; A chart object made using y2artisan or ms_charts
#' @export
#' @examples
#'
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = TRUE)
#'
#' color_settings <- list('blue')
#' text_settings<- list('result' = officer::fp_text(font.size = 20))
#'
#' chart <- ms_single_y2()
#' ms_print_y2()

ms_print_y2 <- function(
  chart_name = chart
){

  chart <- NULL

  chart_name %>%
    print(
      preview = TRUE
    )

}
