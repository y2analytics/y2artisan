#### Add 5 Charts ####
#' Add 5 charts to PowerPoint slide
#'
#' This function adds 5 vertical charts/tables/other objects to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "centerleft"; "center"; centerright"; "right". The chart layout is always 5 tall charts
#' @param label_first_only DEFAULT = FALSE; Set to TRUE if only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' \dontrun{
#' # First create a chart that you can add into a powerpoint object
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = TRUE)
#' color_settings <- list('blue')
#' text_settings<- list('result' = officer::fp_text(font.size = 20))
#' chart_name <- y2artisan::ms_single_y2()
#'
#' # Then before adding additional slides, charts, or tables onto a powerpoint,
#' # you must first read a PowerPoint into R
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1s_y2()
#' doc <- add5c_y2(chart_name, 'left')
#' doc <- add5c_y2(chart_name, 'centerleft')
#' doc <- add5c_y2(chart_name, 'center')
#' doc <- add5c_y2(chart_name, 'centerright')
#' doc <- add5c_y2(chart_name, 'right')
#'
#' print(doc, '~/Desktop/test.pptx')
#' }



### Function
add5c_y2 <- function(
  name,
  position,
  label_first_only = FALSE
) {
  if(label_first_only == FALSE){
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'left' ~ -.125,
          position == 'centerleft' ~ 2.5,
          position == 'center' ~ 5.125,
          position == 'centerright' ~ 7.75,
          position == 'right' ~ 10.375
        ),
        top = dplyr::case_when(
          position == 'left' ~ 1.8,
          position == 'centerleft' ~ 1.8,
          position == 'center' ~ 1.8,
          position == 'centerright' ~ 1.8,
          position == 'right' ~ 1.8
        ),
        height = dplyr::case_when(
          position == 'left' ~ 5.6,
          position == 'centerleft' ~ 5.6,
          position == 'center' ~ 5.6,
          position == 'centerright' ~ 5.6,
          position == 'right' ~ 5.6
        ),
        width = dplyr::case_when(
          position == 'left' ~ 2.875,
          position == 'centerleft' ~ 2.875,
          position == 'center' ~ 2.875,
          position == 'centerright' ~ 2.875,
          position == 'right' ~ 2.875
        )
      )
    )
  } else{ #label_first == TRUE
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'left' ~ .0,
          position == 'centerleft' ~ 4.4375,
          position == 'center' ~ 6.625,
          position == 'centerright' ~ 8.8125,
          position == 'right' ~ 11
        ),
        top = dplyr::case_when(
          position == 'left' ~ 1.8,
          position == 'centerleft' ~ 1.8,
          position == 'center' ~ 1.8,
          position == 'centerright' ~ 1.8,
          position == 'right' ~ 1.8
        ),
        height = dplyr::case_when(
          position == 'left' ~ 5.6,
          position == 'centerleft' ~ 5.6,
          position == 'center' ~ 5.6,
          position == 'centerright' ~ 5.6,
          position == 'right' ~ 5.6
        ),
        width = dplyr::case_when(
          position == 'left' ~ 4.5,
          position == 'centerleft' ~ 2.375,
          position == 'center' ~ 2.375,
          position == 'centerright' ~ 2.375,
          position == 'right' ~ 2.375
        )
      )
    )
  }
}

