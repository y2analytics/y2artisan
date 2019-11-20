#### Add 3 Charts ####
### Description
#' Add 3 charts to PowerPoint slide
#'
#' This function adds 3 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "center"; "right"; "bottomright"; "topright". The chart layout can either be left-center-right OR left-topright-bottomright.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' # First create a chart that you can add into a powerpoint object
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#' color_settings <- list('blue')
#' text_settings<- list('result' = fp_text(font.size = 20))
#' chart_name <- ms_single_y2()
#'
#' # Then before adding additional slides, charts, or tables onto a powerpoint, you must first read a powerpoint into R
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/Resources/Qualtrics Template New.pptx')
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



### Function
add3c_y2 <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F) {
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'topright' ~ 4.375,
        position == 'righttop' ~ 4.375,
        position == 'bottomright' ~ 4.375,
        position == 'rightbottom' ~ 4.375,
        position == 'left' ~ .0,
        position == 'center' ~ 4.25,
        position == 'right' ~ 8.5

      ),
      top = dplyr::case_when(
        position == 'topright' ~ 2,
        position == 'righttop' ~ 2,
        position == 'bottomright' ~ 4.25,
        position == 'rightbottom' ~ 4.25,
        position == 'left' ~ 2,
        position == 'center' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 2.75,
        position == 'righttop' ~ 2.75,
        position == 'bottomright' ~ 2.75,
        position == 'rightbottom' ~ 2.75,
        position == 'left' ~ 5,
        position == 'center' ~ 5,
        position == 'right' ~ 5
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
  } else{ #label_first_only == T
    mschart::ph_with_chart_at(
      doc,
      chart = name,
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
        position == 'topright' ~ 2,
        position == 'righttop' ~ 2,
        position == 'bottomright' ~ 4.25,
        position == 'rightbottom' ~ 4.25,
        position == 'left' ~ 2,
        position == 'center' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 2.75,
        position == 'righttop' ~ 2.75,
        position == 'bottomright' ~ 2.75,
        position == 'rightbottom' ~ 2.75,
        position == 'left' ~ 5,
        position == 'center' ~ 5,
        position == 'right' ~ 5
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
  }
}

