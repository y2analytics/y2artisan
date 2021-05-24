#### Add 6 Charts ####
### Description
#' Add 6 charts to PowerPoint slide
#'
#' This function adds 6 charts/tables/other objects to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "topcenter"; "bottomcenter". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @param label_first_only DEFAULT = FALSE; Set to TTRUE when only the first charts on the left has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
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
#' doc <- add6c_y2(chart_name, 'topleft')
#' doc <- add6c_y2(chart_name, 'bottomleft')
#' doc <- add6c_y2(chart_name, 'topcenter')
#' doc <- add6c_y2(chart_name, 'bottomcenter')
#' doc <- add6c_y2(chart_name, 'topright')
#' doc <- add6c_y2(chart_name, 'bottomright')
#'
#' print(doc, '~/Desktop/test.pptx')
#' }



### Function
add6c_y2 <- function(
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
          position == 'topright' ~ 8.5,
          position == 'bottomright' ~ 8.5,
          position == 'topleft' ~ 0,
          position == 'bottomleft' ~ 0,
          position == 'topcenter' ~ 4.25,
          position == 'bottomcenter' ~ 4.25
        ),
        top = dplyr::case_when(
          position == 'topright' ~ 1.8,
          position == 'bottomright' ~ 4.375,
          position == 'topleft' ~ 1.8,
          position == 'bottomleft' ~ 4.375,
          position == 'topcenter' ~ 1.8,
          position == 'bottomcenter' ~ 4.375
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 3,
          position == 'bottomright' ~ 3,
          position == 'topleft' ~ 3,
          position == 'bottomleft' ~ 3,
          position == 'topcenter' ~ 3,
          position == 'bottomcenter' ~ 3
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 4.5,
          position == 'bottomright' ~ 4.5,
          position == 'topleft' ~ 4.5,
          position == 'bottomleft' ~ 4.5,
          position == 'topcenter' ~ 4.5,
          position == 'bottomcenter' ~ 4.5
        )
      )
    )
  } else{ #label_first == T
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'topright' ~ 9.75,
          position == 'bottomright' ~ 9.75,
          position == 'topleft' ~ 0,
          position == 'bottomleft' ~ 0,
          position == 'topcenter' ~ 6.25,
          position == 'bottomcenter' ~ 6.25
        ),
        top = dplyr::case_when(
          position == 'topright' ~ 1.8,
          position == 'bottomright' ~ 4.375,
          position == 'topleft' ~ 1.8,
          position == 'bottomleft' ~ 4.375,
          position == 'topcenter' ~ 1.8,
          position == 'bottomcenter' ~ 4.25
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 3,
          position == 'bottomright' ~ 3,
          position == 'topleft' ~ 3,
          position == 'bottomleft' ~ 3,
          position == 'topcenter' ~ 3,
          position == 'bottomcenter' ~ 3
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 3.625,
          position == 'bottomright' ~ 3.625,
          position == 'topleft' ~ 6.25,
          position == 'bottomleft' ~ 6.25,
          position == 'topcenter' ~ 3.625,
          position == 'bottomcenter' ~ 3.625
        )
      )
    )
  }
}

