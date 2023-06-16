#### Add 1 Chart ####
### Description
#' Add PowerPoint slide & 1 chart
#'
#' This function adds a new PowerPoint slide and fits 1 chart/table/other object onto it. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param title DEFAULT: "Title"; Add a slide title in quotes, automatically formatted to the Default Qualtrics template
#' @param commentary DEFAULT: "Commentary"; Add commentary/desctiption in quotes, automatically formatted to the Default Qualtrics template
#' @param footer DEFAULT: "Footer"; Add a footer in quotes
#' @param add_slide DEFAULT = TRUE; Automatically adds a blank slide for the chart/table/other object to be added onto. If F, no new slide is added
#' @param text_boxes DEFAULT = FALSE; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT = "Findings / 1 chart"; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT = "Office Theme"; The name of the PP master layout that the slide_name comes from
#' @param title_color DEFAULT: 'Black'; Color or hexcode for slide title
#' @param commentary_color DEFAULT: 'Black'; Color or hexcode for slide commentary
#' @param footer_color DEFAULT: 'Black'; Color or hexcode for slide footer
#' @param font_family DEFAULT: 'BentonSans Regular'; font for slide title, commentary, and footer
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
#' # you must first read a PowerPoint into R
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
#'
#' # Now start adding in your charts
#' doc <- add1c_y2(my_chart)
#'
#' print(doc, '~/Desktop/test.pptx')
#' }

### Function
add1c_y2 <- function(
  name,
  title = 'Title',
  commentary = 'Commentary',
  footer = 'Footer',
  add_slide = TRUE,
  text_boxes = TRUE,
  slide_name = "Blank",
  master_name = "1_Office Theme",
  title_color = 'Black',
  commentary_color = 'Black',
  footer_color = 'Black',
  font_family = 'BentonSans Regular',
  left_start = .5,
  top_start = 1.8,
  height = 5.6,
  width = 12.25
) {
  if (add_slide == TRUE) {
    doc <- add1s_y2(
      title,
      commentary,
      footer,
      text_boxes,
      slide_name,
      master_name,
      title_color,
      commentary_color,
      footer_color,
      font_family
      )
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


