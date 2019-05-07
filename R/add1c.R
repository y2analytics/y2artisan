#### Add 1 Chart ####
### Description
#' Add PowerPoint slide & 1 chart
#'
#' This function adds a new PowerPoint slide and fits 1 chart onto it. It automatically fits the location to the center of the slide.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param add_slide DEFAULT = T; Automatically adds a blank slide for the chart to be added onto. If F, no new slide is added
#' @param text_boxes DEFAULT = T; Automatically adds title and commentary boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT: "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords chart
#' @export
#' @examples
#' doc <- add1c(my_ms_chart)
#' OR
#' doc <- add1c(my_ms_chart, slide_name = "1_Blank", master = "Custom Design")

### Function
add1c <- function(
  name,
  add_slide = T,
  text_boxes = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = 5.5,
  width = 12
) {
  if(add_slide == T) {
    doc <- add1s(text_boxes, slide_name, master_name)
  } else {
    doc <- doc
  }
  mschart::ph_with_chart_at(
    doc,
    chart = name,
    left = left_start,
    top = top_start,
    height = height,
    width = width)
}


