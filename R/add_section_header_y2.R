#### Add section header ####
### Description
#' Add Blank PowerPoint Section Header
#'
#' This function adds a new PowerPoint section header along with a title box
#' @param title DEFAULT: "Title"; Add a title in quotes here to have your Section Headers put in for you by R
#' @param n DEFAULT: "1"; The number added to your section header (for Qualtrics style reports only). If section 2, put "2", etc.
#' @param report_style DEFAULT: "qualtrics"; The report style/template you are using -- must be either 'qualtrics', 'municipal', or 'y2'
#' @param slide_name DEFAULT: "Section Header; The name of the type of the PP slide you want added to the PP
#' @param master_name DEFAULT: NULL; The name of the PP master layout that the slide_name comes from. If no argument provided and the report style is 'qualtrics', defaults to "1_Office Theme"; if no argument provided and the report style is 'municipal' or 'y2', defaults to "Office Theme".
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
    report_style = c('qualtrics', 'municipal', 'y2'),
    master_name = NULL,
    slide_name = NULL,
    text_boxes = TRUE
) {
  
  # Test matching arguments
  report_style <- rlang::arg_match(report_style)
  
  # Null (default) master names
  if (is.null(master_name) & report_style == 'qualtrics') {
    master_name <- '1_Office Theme'
  } else if (is.null(master_name) & report_style != 'qualtrics') {
    master_name <- 'Office Theme'
  }
  
  # Null (default) section header names
  if (is.null(slide_name) & report_style == 'qualtrics') {
    slide_name <- 'XM Sidebar (1)'
  } else if (is.null(master_name) & report_style != 'qualtrics')  {
    slide_name <- 'Section Header'
  } 
  
  # New slide with section header text
  doc <- officer::add_slide(
    doc,
    layout = slide_name,
    master = master_name
  )
  
  if (text_boxes == TRUE) {
    
    if (report_style == 'qualtrics') {
      
      doc <- doc %>% add_number_section_header(n)
      doc <- doc %>% add_title_section_header(title, report_style)
      
    } else {
      
      doc <- doc %>% add_title_section_header(title, report_style)
      
    }
    
  }
  
}


# Private functions -------------------------------------------------------

# Only for Qualtrics style reports
add_number_section_header <- function(
    doc,
    text
){
  
  properties <- officer::fp_text(
    color = 'white',
    font.size = 139,
    font.family = 'BentonSans Regular',
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
      left = 0.85,
      top = 0.27,
      width = 2,
      height = 2
    )
  )
  
}

add_title_section_header <- function(
    doc,
    text,
    report_style
){
  
  if (report_style == 'qualtrics') {
    
    properties <- officer::fp_text(
      color = 'black',
      font.size = 60,
      font.family = 'BentonSans Regular'
    )
    
    slide_subtitle <- officer::ftext(
      text,
      properties
    )
    
    officer::ph_with(
      doc,
      value = officer::fpar(slide_subtitle),
      location = officer::ph_location(
        left = 2.55,
        top = 0.75,
        width = 11,
        height = 1
      )
    )
    
  } else if (report_style == 'municipal') {
    
    properties <- officer::fp_text(
      color = 'white',
      font.size = 66,
      font.family = 'Flama Medium'
    )
    
    slide_subtitle <- officer::ftext(
      text,
      properties
    )
    
    officer::ph_with(
      doc,
      value = officer::fpar(
        slide_subtitle,
        fp_p = officer::fp_par(
          text.align = 'center'
        )
      ),
      location = officer::ph_location(
        left = 1,
        top = 3.14,
        width = 11.34,
        height = 1.21
      )
    )
    
  } else if (report_style == 'y2') {
    
    properties <- officer::fp_text(
      color = '#E8903E',
      font.size = 60,
      font.family = 'Flama Condensed Bold',
      bold = TRUE
    )
    
    slide_subtitle <- officer::ftext(
      text,
      properties
    )
    
    officer::ph_with(
      doc,
      value = officer::fpar(
        slide_subtitle,
        fp_p = officer::fp_par(
          text.align = 'center'
        )
      ),
      location = officer::ph_location(
        left = 0,
        top = 3.18,
        width = 13.33,
        height = 1.14
      )
    )
    
  }
  
}
