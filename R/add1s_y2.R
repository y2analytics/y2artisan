#### ***** MS CHARTS ***** ####
#### Add 1 Slide ####
### Description
#' Add 1 Blank PowerPoint slide
#'
#' This function adds a new PowerPoint slide along with a title and commentary box to the current pp object in R
#' @param text_boxes DEFAULT = T; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT: "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords powerpoint slide
#' @export
#' @examples
#' doc <- add1s_y2()


add1s_y2 <- function(
  text_boxes = T,
  slide_name = "Findings / 1 chart",
  master_name = "Office Theme"
) {
doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )
  if(text_boxes == T){
    doc <- officer::ph_with_text(
      doc,
      type = 'title',
      str = 'xxx'
    )
    doc <- officer::ph_with_text(
      doc,
      type = 'body',
      str = 'xxx'
    )
    doc <- officer::ph_with_text(
      doc,
      type = 'ftr',
      str = 'Q: '
    )
  } else{
    doc <- doc
  }
}

