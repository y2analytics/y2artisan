#### Add section header ####
### Description
#' Add Blank PowerPoint Section Header
#'
#' This function adds a new PowerPoint section header along with a title box
#' @param title DEFAULT: "Title"; Add a title in quotes here to have your Section Headers put in for you by R
#' @param n DEFAULT: "1"; The number added to your section header. If section 2, put "2", etc.
#' @param slide_name DEFAULT: "Section Header"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @param text_boxes DEFAULT = TRUE; Automatically adds a title and section number. Set to FALSE for no text boxes
#' @keywords powerpoint slide
#' @export
#' @examples
#' # Before adding additional slides, charts, or tables onto a powerpoint,
#' # you must first read a powerpoint into R
#' \dontrun{
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
#' doc <- add_section_header_y2()
#' print(doc, '~/Desktop/test.pptx')
#' }

add_section_header_y2 <- function(
  title = 'Title',
  n = '1',
  slide_name = "XM Sidebar (1)",
  master_name = "1_Office Theme",
  text_boxes = TRUE
) {
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )

  if (text_boxes == TRUE) {
  doc <- doc %>% add_number_section_header(n)
  doc <- doc %>% add_title_section_header(title)
  } else {
    doc <- doc
  }
}



# Private functions -------------------------------------------------------

add_number_section_header <- function(
  doc,
  text,
  text_color = "white",
  font_size = 139,
  font_family = "BentonSans Regular",
  left = .85,
  top = 0.27,
  width = 2,
  height = 2
){
  properties <- officer::fp_text(
    color = text_color,
    font.size = font_size,
    font.family = font_family,
    bold = TRUE
  )

  slide_title <- officer::ftext(
    text,
    properties
  )

  officer::ph_with(
    doc,
    value = officer::fpar(slide_title),
    location = officer::ph_location(
      left = left,
      top = top,
      width = width,
      height = height
    )
  )
}

add_title_section_header <- function(
  doc,
  text,
  text_color = "Black",
  font_size = 60,
  font_family = "BentonSans Regular",
  left = 2.55,
  top = .75,
  width = 11,
  height = 1
){
  properties <- officer::fp_text(
    color = text_color,
    font.size = font_size,
    font.family = font_family
  )

  slide_subtitle <- officer::ftext(
    text,
    properties
  )

  officer::ph_with(
    doc,
    value = officer::fpar(slide_subtitle),
    location = officer::ph_location(
      left = left,
      top = top,
      width = width,
      height = height
    )
  )
}

