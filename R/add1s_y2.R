#### Add 1 Slide ####
### Description
#' Add 1 Blank PowerPoint slide
#'
#' This function adds a new PowerPoint slide. For this function to work, you need a powerpoint object saved into R called 'doc'
#' @param title DEFAULT: 'Title'; Add a slide title in quotes, automatically formatted to the specified report style template
#' @param commentary DEFAULT: 'Commentary'; Add commentary/desctiption in quotes, automatically formatted to the specified report style template
#' @param footer DEFAULT: 'Footer'; Add a footer in quotes
#' @param report_style DEFAULT: "qualtrics"; The report style/template you are using -- must be either 'qualtrics', 'municipal', or 'y2'
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
    report_style = c('qualtrics', 'municipal', 'y2'),
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
  if (is.null(master_name) & report_style == 'qualtrics') {
    master_name <- '1_Office Theme'
  } else if (is.null(master_name) & report_style != 'qualtrics') {
    master_name <- 'Office Theme'
  }
  
  if (is.null(title_color) & report_style == 'qualtrics') {
    title_color <- 'black'
  } else if (is.null(title_color) & report_style == 'municipal') {
    title_color <- 'white'
  } else if (is.null(title_color) & report_style == 'y2') {
    title_color <- '#E8903E'
  }
  
  if (is.null(commentary_color) & report_style == 'qualtrics') {
    commentary_color <- 'black'
  } else if (is.null(commentary_color) & report_style == 'municipal') {
    commentary_color <- 'white'
  } else if (is.null(commentary_color) & report_style == 'y2') {
    commentary_color <- '#767171'
  }
  
  if (is.null(footer_color) & report_style == 'qualtrics') {
    footer_color <- 'black'
  } else if (is.null(footer_color) & report_style == 'municipal') {
    footer_color <- '#222222'
  } else if (is.null(footer_color) & report_style == 'y2') {
    footer_color <- '#A6A6A6'
  }
  
  if (is.null(font_title) & report_style == 'qualtrics') {
    font_title <- 'BentonSans Regular'
  } else if (is.null(font_title) & report_style == 'municipal') {
    font_title <- 'Flama Medium'
  } else if (is.null(font_title) & report_style == 'y2') {
    font_title <- 'Flama Condensed Bold'
  }
  
  if (is.null(font_text) & report_style == 'qualtrics') {
    font_text <- 'BentonSans Regular'
  } else if (is.null(font_text) & report_style == 'municipal') {
    font_text <- 'Flama Light'
  } else if (is.null(font_text) & report_style == 'y2') {
    font_text <- 'Flama Semicondensed Basic'
  }
  
  if (is.null(footer_left) & report_style == 'qualtrics') {
    footer_left <- 0.5
  } else if (is.null(footer_left) & report_style == 'municipal') {
    footer_left <- 0.24
  } else if (is.null(footer_left) & report_style == 'y2') {
    footer_left <- 0
  }
  
  if (is.null(footer_top) & report_style == 'qualtrics') {
    footer_top <- 7
  } else if (is.null(footer_top) & report_style == 'municipal'){
    footer_top <- 7.1
  } else if (is.null(footer_top) & report_style == 'y2') {
    footer_top <- 7.23
  }
  
  if (is.null(footer_width) & report_style == 'qualtrics') {
    footer_width <- 10.5
  } else if (is.null(footer_width) & report_style == 'municipal') {
    footer_width <- 12.76
  } else if (is.null(footer_width) & report_style == 'y2') {
    footer_width <- 12.46
  }
  
  if (is.null(footer_height) & report_style == 'qualtrics') {
    footer_height <- 0.5
  } else if (is.null(footer_height) & report_style == 'municipal') {
    footer_height <- 0.4
  } else if (is.null(footer_height) & report_style == 'y2') {
    footer_height <- 0.27
  }
  
  if (report_style == 'qualtrics' | report_style == 'y2') {

    doc <- officer::add_slide(
      doc,
      layout = slide_name,
      master = master_name
    )
    
    if (text_boxes == TRUE) {
      
      doc <- doc %>% add_title_findings(title, title_color, font_title, report_style)
      doc <- doc %>% add_commentary_findings(commentary, commentary_color, font_text, report_style)
      doc <- doc %>% add_footer_findings(footer, footer_color, font_text, report_style, footer_left, footer_top, footer_width, footer_height)
      
    }
    
  } else if (report_style == 'municipal') {
    
    doc <- officer::add_slide(doc)
    
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
    
  } else if (report_style == 'municipal') {
    
    properties <- officer::fp_text(
      color = title_color,
      font.size = 40,
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
        left = 0,
        top = 0,
        width = 13.33,
        height = 0.83,
        bg = title_bg_color
      )
    )
    
  } else if (report_style == 'y2') {
    
    properties <- officer::fp_text(
      color = title_color,
      font.size = 44,
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
        left = 0,
        top = 0.05,
        width = 13.33,
        height = 0.88
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
    
  } else if (report_style == 'municipal') {
    
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
        left = 0,
        top = 0.83,
        width = 13.33,
        height = 0.83,
        bg = commentary_bg_color
      )
    )
    
  } else if (report_style == 'y2') {
    
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
        left = 0.16,
        top = 1.1,
        width = 12.82,
        height = 0.34,
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
