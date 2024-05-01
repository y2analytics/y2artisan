#### Add 1 Slide ####
### Description
#' Add 1 Blank PowerPoint slide
#'
#' This function adds a new PowerPoint slide. For this function to work, you need a powerpoint object saved into R called 'doc'
#' @param title DEFAULT: 'Title'; Add a slide title in quotes, automatically formatted to the specified report style template
#' @param commentary DEFAULT: 'Commentary'; Add commentary/desctiption in quotes, automatically formatted to the specified report style template
#' @param footer DEFAULT: 'Footer'; Add a footer in quotes
#' @param text_boxes DEFAULT = TRUE; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes
#' @param slide_name DEFAULT: 'Blank'; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: '1_Office Theme'; The name of the PP master layout that the slide_name comes from
#' @param title_color DEFAULT: NULL; Color or hexcode for slide title. If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports
#' @param commentary_color DEFAULT: NULL; Color or hexcode for slide commentary If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports
#' @param footer_color DEFAULT: NULL; Color or hexcode for slide footer. If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports
#' @param font_title DEFAULT: NULL; font for slide title. If no value provided, will be set to 'BentonSans Regular' for Qualtrics style reports and 'Flama Medium' for Municipal style reports
#' @param font_text DEFAULT: NULL; font for slide text boxes and footer. If no value provided, will be set to 'BentonSans Regular' for Qualtrics style reports and 'Flama Light' for Municipal style reports
#' @param title_bg_color DEFAULT: '#1A497A; Background color of slide title text box (only used for Municipal style reports)
#' @param commentary_bg_color DEFAULT: '#9EBCDB; Background color of slide commentary text box (only used for Municipal style reports)
#' @param footer_left DEFAULT = NULL; how far footer will start from left of slide. If no value provided, defaults to approximately 0.35 for either report style
#' @param footer_top DEFAULT = NULL; how far footer will start from top of slide. If no value provided, defaults to approximately 7 for either report style
#' @param footer_width DEFAULT = NULL; height of footer on slide. If no value provided, defaults to approximately 11.5 for either report style
#' @param footer_height DEFAULT = NULL; width of footer on slide. If no value provided, defaults to approximately 0.5 for either report style
#' @keywords powerpoint slide
#' @export
#' @examples
#'
#' # Before adding slides, charts, or tables onto a powerpoint,
#' # you must first read a PowerPoint into R
#' \dontrun{
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
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
    report_style = c('qualtrics', 'municipal'),
    text_boxes = TRUE,
    slide_name = 'Blank',
    master_name = '1_Office Theme',
    title_color = NULL,
    commentary_color = NULL,
    footer_color = NULL,
    font_title = NULL,
    font_text = NULL,
    title_bg_color = '#1A497A',
    commentary_bg_color = '#9EBCDB',
    footer_left = NULL,
    footer_top = NULL,
    footer_width = NULL,
    footer_height = NULL
) {
  
  # Test matching arguments
  report_style <- rlang::arg_match(report_style)
  
  # Set NULL values from report style
  title_color <- dplyr::case_when(
    is.null(title_color) & report_style == 'qualtrics' ~ 'black',
    is.null(title_color) & report_style == 'municipal' ~ 'white',
    TRUE ~ title_color
  )
  
  commentary_color <- dplyr::case_when(
    is.null(commentary_color) & report_style == 'qualtrics' ~ 'black',
    is.null(commentary_color) & report_style == 'municipal' ~ 'white',
    TRUE ~ commentary_color
  )
  
  footer_color <- dplyr::case_when(
    is.null(footer_color) & report_style == 'qualtrics' ~ 'black',
    is.null(footer_color) & report_style == 'municipal' ~ '#222222',
    TRUE ~ footer_color
  )
  
  font_title <- dplyr::case_when(
    is.null(font_title) & report_style == 'qualtrics' ~ 'BentonSans Regular',
    is.null(font_title) & report_style == 'municipal' ~ 'Flama Medium',
    TRUE ~ font_title
  )
  
  font_text <- dplyr::case_when(
    is.null(font_text) & report_style == 'qualtrics' ~ 'BentonSans Regular',
    is.null(font_text) & report_style == 'municipal' ~ 'Flama Light',
    TRUE ~ font_text
  )
  
  footer_left <- dplyr::case_when(
    is.null(footer_left) & report_style == 'qualtrics' ~ 0.5,
    is.null(footer_left) & report_style == 'municipal' ~ 0.24,
    TRUE ~ footer_left
  )
  
  footer_top <- dplyr::case_when(
    is.null(footer_top) & report_style == 'qualtrics' ~ 7,
    is.null(footer_top) & report_style == 'municipal' ~ 7.1,
    TRUE ~ footer_top
  )
  
  footer_width <- dplyr::case_when(
    is.null(footer_width) & report_style == 'qualtrics' ~ 10.5,
    is.null(footer_width) & report_style == 'municipal' ~ 12.76,
    TRUE ~ footer_width
  )
  
  footer_height <- dplyr::case_when(
    is.null(footer_height) & report_style == 'qualtrics' ~ 0.5,
    is.null(footer_height) & report_style == 'municipal' ~ 0.4,
    TRUE ~ footer_height
  )
  
  # New blank slide
  if (report_style == 'qualtrics') {
    
    doc <- officer::add_slide(
      doc,
      layout = slide_name,
      master = master_name
    )
    
    # Add text boxes if specified
    if (text_boxes == TRUE) {
      
      doc <- doc %>% add_title_findings(title, title_color, font_title, report_style)
      doc <- doc %>% add_commentary_findings(commentary, commentary_color, font_text, report_style)
      doc <- doc %>% add_footer_findings(footer, footer_color, font_text, report_style)
      
    }
    
  } else {
    
    doc <- officer::add_slide(doc)
   
    # Add text boxes if specified
    if (text_boxes == TRUE) {
      
      doc <- doc %>% add_title_findings(title, title_color, font_title, report_style, title_bg_color)
      doc <- doc %>% add_commentary_findings(commentary, commentary_color, font_text, report_style, commentary_bg_color)
      doc <- doc %>% add_footer_findings(footer, footer_color, font_text, report_style, footer_left, footer_top, footer_width, footer_height)
      
    }

  }
  
}


# Private functions -------------------------------------------------------

add_title_findings <- function(
    doc,
    text,
    title_color,
    font_family,
    report_style,
    title_bg_color = NULL
){
  
  # Qualtrics style slide title
  if (report_style == 'qualtrics') {
    
    properties <- officer::fp_text(
      color = title_color,
      font.size = 32,
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
        left = 0.5,
        top = 0.25,
        width = 10.5,
        height = 0.75
      )
    )
    
  # Municipal style slide title
  } else {
    
    properties <- officer::fp_text(
      color = title_color,
      font.size = 40,
      font.family = font_family
    )
    
    slide_title <- officer::ftext(
      text,
      properties
    )
    
    ph_with(
      doc,
      value = officer::fpar(slide_title),
      location = officer::ph_location(
        left = 0,
        top = 0,
        width = 13.33,
        height = 0.83,
        bg = title_bg_color
      )
    )
    
  }
  
}

add_commentary_findings <- function(
    doc,
    text,
    commentary_color,
    font_family,
    report_style,
    commentary_bg_color = NULL
){
  
  # Qualtrics style slide comm. 
  if (report_style == 'qualtrics') {
    
    properties <- officer::fp_text(
      color = commentary_color,
      font.size = 14,
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
        left = 0.5,
        top = 1,
        width = 10.5,
        height = 0.75
      )
    )
    
  # Municipal style slide comm.
  } else {
    
    properties <- officer::fp_text(
      color = commentary_color,
      font.size = 14,
      font.family = font_family
    )
    
    slide_commentary <- officer::ftext(
      text,
      properties
    )
    
    ph_with(
      doc,
      value = officer::fpar(slide_commentary),
      location = officer::ph_location(
        left = 0,
        top = 0.83,
        width = 13.33,
        height = 0.83,
        bg = commentary_bg_color
      )
    )
    
  }
  
}

add_footer_findings <- function(
    doc,
    text,
    footer_color,
    font_family,
    report_style,
    footer_left,
    footer_top,
    footer_width,
    footer_height
){
  
  properties <- officer::fp_text(
    color = footer_color,
    font.size = 10,
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
      left = footer_left,
      top = footer_top,
      width = footer_width,
      height = footer_height
    )
  )
  
}
