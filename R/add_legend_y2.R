#### Add Custom (Table/Flextable) Legend ####
### Description
#' Add a Custom PPT Table Legend
#'
#' This function adds a custom legend of specified labels and color palette to the last slide in your report. For this function to work, you need a powerpoint object saved into R called 'doc'
#' @param labels A vector of labels to use to create your legend
#' @param palette A color palette to use as the keys for your legend
#' @param font_family DEFAULT: 'Flama Medium'; font for the legend labels
#' @param font_size DEFAULT: 12; font size for legend labels
#' @param label_color DEFAULT: 'white'; text color for the legend labels
#' @param position DEFAULT: 'center'; rough position on the slide where the legend will be placed. Must be either 'center', 'left', or 'right'.
#' @keywords powerpoint slide
#' @export
#' @examples
#'
#' # Before adding a legend onto a powerpoint,
#' # you must first read a PowerPoint into R
#' \dontrun{
#' doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
#' Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
#'
#' # Add a blank slide
#' doc <- add1s_y2()
#'
#' # Add a legend
#' doc <- add_legend_y2(
#'   labels = c(
#'     'Agree', 
#'     'Neutral', 
#'     'Disagree'
#'   ),
#'   palette = c(
#'     'blue',
#'     'gray',
#'     'red'
#'   )
#')
#'
#' print(doc, '~/Desktop/test.pptx')
#' }

## Add table legend function
add_legend_y2 <- function(
    labels,
    palette,
    font_family = 'Flama Medium',
    font_size = 12,
    label_color = 'white',
    position = 'center'
) {
  
  # Turn labels into dataframe
  labels <- labels %>% 
    dplyr::tibble() %>% 
    t() %>% 
    as.data.frame()
  
  # Create flextable legend
  legend <- flextable::flextable(labels) %>% 
    flextable::bg(
      j = colnames(labels), 
      bg = palette
    ) %>%
    flextable::delete_part('header') %>% 
    flextable::border_remove() %>% 
    flextable::align(
      align = 'center', 
      part = 'all'
    ) %>% 
    flextable::color(
      color = label_color, 
      part = 'all'
    ) %>% 
    flextable::font(
      fontname = font_family,
      part = 'all'
    ) %>% 
    flextable::fontsize(
      size = font_size,
      part = 'all'
    ) %>% 
    flextable::height(height = 0.5) %>% 
    flextable::width(width = 1.5)
  
  # Send to position of doc
  if (position == 'center') {
    
    officer::ph_with(
      doc,
      value = legend,
      location = officer::ph_location(
        left = 3,
        top = 2.125,
      )
    )
    
  } else if (position == 'left') {
    
    officer::ph_with(
      doc,
      value = legend,
      location = officer::ph_location_left()
    )
    
  } else if (position == 'right') {
    
    officer::ph_with(
      doc,
      value = legend,
      location = officer::ph_location_right()
    )
    
  }
  
}
