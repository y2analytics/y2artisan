#### Add 4 Charts ####
### Description
#' Add 4 charts to PowerPoint slide
#'
#' This function adds 4 charts/tables/other objects to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "left"; "centerleft"; "centerright"; "right". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @param label_first_only DEFAULT = FALSE; Set to TRUE if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
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
#' # you must first read a powerpoint into R
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/
#' Resources/Qualtrics Template New.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1s_y2()
#' doc <- add4c_y2(chart_name, 'topright')
#' doc <- add4c_y2(chart_name, 'bottomright')
#' doc <- add4c_y2(chart_name, 'topleft')
#' doc <- add4c_y2(chart_name, 'bottomleft')
#'
#' doc <- add1s_y2()
#' doc <- add4c_y2(chart_name, 'left')
#' doc <- add4c_y2(chart_name, 'centerleft')
#' doc <- add4c_y2(chart_name, 'centerright')
#' doc <- add4c_y2(chart_name, 'right')
#'
#' print(doc, '~/Desktop/test.pptx')
#' }


### Function
add4c_y2 <- function(
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
          position == 'bottomright' ~ 4.25,
          position == 'topleft' ~ 2,
          position == 'bottomleft' ~ 4.25,
          position == 'left' ~ 2,
          position == 'centerleft' ~ 2,
          position == 'centerright' ~ 2,
          position == 'right' ~ 2
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 2.75,
          position == 'bottomright' ~ 2.75,
          position == 'topleft' ~ 2.75,
          position == 'bottomleft' ~ 2.75,
          position == 'left' ~ 5,
          position == 'centerleft' ~ 5,
          position == 'centerright' ~ 5,
          position == 'right' ~ 5
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
      )
    )
  } else{ #label_first == T
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
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
          position == 'bottomright' ~ 4.25,
          position == 'topleft' ~ 2,
          position == 'bottomleft' ~ 4.25,
          position == 'left' ~ 2,
          position == 'centerleft' ~ 2,
          position == 'centerright' ~ 2,
          position == 'right' ~ 2
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 2.75,
          position == 'bottomright' ~ 2.75,
          position == 'topleft' ~ 2.75,
          position == 'bottomleft' ~ 2.75,
          position == 'left' ~ 5,
          position == 'centerleft' ~ 5,
          position == 'centerright' ~ 5,
          position == 'right' ~ 5
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
    )
  }
}
