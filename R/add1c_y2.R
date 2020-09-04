#### Add 1 Chart ####
### Description
#' Add PowerPoint slide & 1 chart
#'
#' This function adds a new PowerPoint slide and fits 1 chart/table/other object onto it. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param add_slide DEFAULT = TRUE; Automatically adds a blank slide for the chart/table/other object to be added onto. If F, no new slide is added
#' @param text_boxes DEFAULT = FALSE; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT = "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT = "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @param left_start DEFAULT = .5; how far chart will start from left of slide
#' @param top_start DEFAULT = 2; how far chart will start from top of slide
#' @param height DEFAULT = 5; height of chart on slide
#' @param width DEFAULT = 12; width of chart on slide
#' @keywords chart table
#' @export
#' @examples
#' \dontrun{
#' # First create a chart that you can add into a powerpoint object
#' frequencies <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#' color_settings <- list('blue')
#' text_settings<- list('result' = officer::fp_text(font.size = 20))
#' my_chart <- y2artisan::ms_single_y2()
#'
#' # Then before adding additional slides, charts, or tables onto a powerpoint,
#' # you must first read a powerpoint into R
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/
#' Resources/Qualtrics Template New.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1c_y2(my_chart)
#'
#' print(doc, '~/Desktop/test.pptx')
#' }

### Function
add1c_y2 <- function(
  name,
  add_slide = TRUE,
  text_boxes = FALSE,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5,
  width = 12
) {
  if(add_slide == TRUE) {
    doc <- add1s_y2(text_boxes, slide_name, master_name)
  } else {
    doc <- doc
  }
  officer::ph_with(
    doc,
    value = name,
    location = officer::ph_location(
      left = left_start,
      top = top_start,
      height = height,
      width = width
    )
  )
}


