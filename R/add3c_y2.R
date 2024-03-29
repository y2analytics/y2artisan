#### Add 3 Charts ####
### Description
#' Add 3 charts to PowerPoint slide
#'
#' This function adds 3 charts/table/other objects to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "center"; "right"; "bottomright"; "topright". The chart layout can either be left-center-right OR left-topright-bottomright.
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
#' # you must first read a PowerPoint into R
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1s_y2()
#' doc <- add3c_y2(chart_name, 'left')
#' doc <- add3c_y2(chart_name, 'center')
#' doc <- add3c_y2(chart_name, 'right')
#'
#' doc <- add1s_y2()
#' doc <- add3c_y2(chart_name, 'topright')
#' doc <- add3c_y2(chart_name, 'bottomright')
#' doc <- add3c_y2(chart_name, 'left')
#'
#' print(doc, '~/Desktop/test.pptx')
#' }


### Function
add3c_y2 <- function(
  name,
  position,
  label_first_only = FALSE
) {
  if (label_first_only == FALSE) {
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'topright' ~ 4.375,
          position == 'righttop' ~ 4.375,
          position == 'bottomright' ~ 4.375,
          position == 'rightbottom' ~ 4.375,
          position == 'left' ~ 0,
          position == 'center' ~ 4.25,
          position == 'right' ~ 8.5
        ),
        top = dplyr::case_when(
          position == 'topright' ~ 1.8,
          position == 'righttop' ~ 1.8,
          position == 'bottomright' ~ 4.375,
          position == 'rightbottom' ~ 4.375,
          position == 'left' ~ 1.8,
          position == 'center' ~ 1.8,
          position == 'right' ~ 1.8
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 3,
          position == 'righttop' ~ 3,
          position == 'bottomright' ~ 3,
          position == 'rightbottom' ~ 3,
          position == 'left' ~ 5.6,
          position == 'center' ~ 5.6,
          position == 'right' ~ 5.6
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 8.615,
          position == 'righttop' ~ 8.615,
          position == 'bottomright' ~ 8.615,
          position == 'rightbottom' ~ 8.615,
          position == 'left' ~ 4.5,
          position == 'center' ~ 4.5,
          position == 'right' ~ 4.5
        )
      )
    )
  } else { #label_first_only == TRUE
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'topright' ~ 4.375,
          position == 'righttop' ~ 4.375,
          position == 'bottomright' ~ 4.375,
          position == 'rightbottom' ~ 4.375,
          position == 'left' ~ .0,
          position == 'center' ~ 6.25,
          position == 'right' ~ 9.75
        ),
        top = dplyr::case_when(
          position == 'topright' ~ 1.8,
          position == 'righttop' ~ 1.8,
          position == 'bottomright' ~ 4.375,
          position == 'rightbottom' ~ 4.375,
          position == 'left' ~ 1.8,
          position == 'center' ~ 1.8,
          position == 'right' ~ 1.8
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 3,
          position == 'righttop' ~ 3,
          position == 'bottomright' ~ 3,
          position == 'rightbottom' ~ 3,
          position == 'left' ~ 5.6,
          position == 'center' ~ 5.6,
          position == 'right' ~ 5.6
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 8.615,
          position == 'righttop' ~ 8.615,
          position == 'bottomright' ~ 8.615,
          position == 'rightbottom' ~ 8.615,
          position == 'left' ~ 6.25,
          position == 'center' ~ 3.625,
          position == 'right' ~ 3.625
        )
      )
    )
  }
}

