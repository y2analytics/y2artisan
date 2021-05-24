#### ***** MS CHARTS ***** ####
#### Add 1 Slide ####
### Description
#' Add 1 Blank PowerPoint slide
#'
#' This function adds a new PowerPoint slide. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param title DEFAULT: "Title"; Add a slide title in quotes, automatically formatted to the Default Qualtrics template
#' @param commentary DEFAULT: "Commentary"; Add commentary/desctiption in quotes, automatically formatted to the Default Qualtrics template
#' @param footer DEFAULT: "Footer"; Add a footer in quotes
#' @param text_boxes DEFAULT = FALSE; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT: "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @keywords powerpoint slide
#' @export
#' @examples
#'
#' # Before adding slides, charts, or tables onto a powerpoint,
#' # you must first read a powerpoint into R
#' \dontrun{
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/
#' Projects/Qualtrics/2021 Qualtrics Template Condensed.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1s_y2()
#'
#' print(doc, '~/Desktop/test.pptx')
#' }


add1s_y2 <- function(
  title = 'Title',
  commentary = 'Commentary',
  footer = 'Footer',
  text_boxes = TRUE,
  slide_name = "Blank",
  master_name = "1_Office Theme"
) {
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )
  if (text_boxes == TRUE) {
    doc <- doc %>% add_title_findings(title)
    doc <- doc %>% add_commentary_findings(commentary)
    doc <- doc %>% add_footer_findings(footer)
  } else {
    doc <- doc
  }
}


# Private functions -------------------------------------------------------

add_title_findings <- function(
  doc,
  text,
  text_color = "Black",
  font_size = 32,
  font_family = "BentonSans Regular",
  left = 0.5,
  top = 0.25,
  width = 10.5,
  height = .75
){
  properties <- officer::fp_text(
    color = text_color,
    font.size = font_size,
    font.family = font_family
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


add_commentary_findings <- function(
  doc,
  text,
  text_color = "Black",
  font_size = 14,
  font_family = "BentonSans Regular",
  left = 0.5,
  top = 1,
  width = 10.5,
  height = .75
){
  properties <- officer::fp_text(
    color = text_color,
    font.size = font_size,
    font.family = font_family
  )

  slide_commentary <- officer::ftext(
    text,
    properties
  )

  officer::ph_with(
    doc,
    value = officer::fpar(slide_commentary),
    location = officer::ph_location(
      left = left,
      top = top,
      width = width,
      height = height
    )
  )
}


add_footer_findings <- function(
  doc,
  text,
  text_color = "Black",
  font_size = 10,
  font_family = "BentonSans Regular",
  left = 0.5,
  top = 7.13,
  width = 10.5,
  height = .25
){
  properties <- officer::fp_text(
    color = text_color,
    font.size = font_size,
    font.family = font_family
  )

  slide_footer <- officer::ftext(
    text,
    properties
  )

  officer::ph_with(
    doc,
    value = officer::fpar(slide_footer),
    location = officer::ph_location(
      left = left,
      top = top,
      width = width,
      height = height
    )
  )
}

