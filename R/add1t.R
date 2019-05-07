#### Add 1 Table - Executive function ####
### Description
#' Add PowerPoint slide & 1 table
#'
#' This function adds a new PowerPoint slide and fits 1 table onto it. It automatically fits the location to the center of the slide. Beware that if the object passed to the function is not a pre-formated flextable, it automatically assigns a background color to the table and sets the fontsize to 18.
#' @param name The name of the dataframe to be added to a new PowerPoint slide.
#' @param add_slide DEFAULT = T; Automatically adds a blank slide for the chart to be added onto. If F, no new slide is added
#' @param text_boxes DEFAULT = T; Automatically adds title and commentary boxes to the blank slide. Set to F for no text boxes
#' @param slide_name The name of the type of the PP slide you want added to the PP. DEFAULT: "Findings / 1 chart"
#' @param master_name The name of the PP master layout that the slide_name comes from. DEFAULT: "Office Theme"
#' @keywords chart
#' @export
#' @examples
#' doc <- add1t(my_flextable)
#' OR
#' doc <- add1t(my_table, slide_name = "1_Blank", master = "Custom Design")


### Function
add1t <- function(
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
  if(class(name)[1] == 'flextable'){
    add1_flextable(name, add_slide, text_boxes, slide_name, master_name, left_start, top_start, height, width)
  } else{
    add1_table(name, add_slide, text_boxes, slide_name, master_name, left_start, top_start, height, width)
  }
}


#### Hidden Functions ####
### Check if flextable

### If NOT flextable
add1_table <- function(
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
  officer::ph_with_table_at(
    doc,
    value = name,
    left = left_start,
    top = top_start,
    height = height,
    width = width)
}

### If YES flextable
add1_flextable <- function(
  name,
  add_slide = T,
  text_boxes = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme",
  left_start = .5,
  top_start = 2,
  height = NULL,
  width = NULL
) {
  if(add_slide == T) {
    doc <- add1s(text_boxes, slide_name, master_name)
  } else {
    doc <- doc
  }
  flextable::ph_with_flextable_at(
    doc,
    value = name,
    left = left_start,
    top = top_start
  )
}

