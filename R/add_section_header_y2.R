#### Add section header ####
### Description
#' Add Blank PowerPoint Section Header
#'
#' This function adds a new PowerPoint section header along with a title box
#' @param title DEFAULT: "xxx"; Add a title in quotes here to have your Section Headers put in for you by R
#' @param slide_name DEFAULT: "Section Header"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords powerpoint slide
#' @export
#' @examples
#' doc <- add_section_header_y2()
#' OR
#' add_section_header_y2()

add_section_header_y2 <- function(
  title = 'xxx',
  slide_name = "Section Header",
  master_name = "Office Theme"
) {
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )
  doc <- officer::ph_with_text(
    doc,
    type = 'title',
    str = title
  )
}

