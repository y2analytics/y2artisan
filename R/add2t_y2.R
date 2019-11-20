### Description
#' Add 2 tables to PowerPoint slide
#'
#' This function adds 2 tables to a PowerPoint slide. The tables are automatically added to the last slide of the PP object in R. Beware that if the object passed to the function is not a pre-formated flextable, it automatically assigns a background color to the table and sets the fontsize to 18.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "top"; "bottom"; "left"; "right". The chart layout can either be top-bottom OR left-right.
#' @keywords chart
#' @export
#' @examples
#' # First create a table that you can add into a powerpoint object
#' my_table <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#'
#' # Then before adding additional slides, charts, or tables onto a powerpoint, you must first read a powerpoint into R
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/Resources/Qualtrics Template New.pptx')
#'
#' # Now start adding in your tables
#' doc <- add1s_y2()
#' doc <- add2t_y2(my_table, 'left')
#' doc <- add2t_y2(my_table, 'right')
#'
#' doc <- add1s_y2()
#' doc <- add2t_y2(my_table, 'top')
#' doc <- add2t_y2(my_table, 'bottom')
#'
#' print(doc, '~/Desktop/test.pptx')


### Function
add2t_y2 <- function(
  name,
  position
) {
  if(class(name)[1] == 'flextable'){
    add2_flextable(name, position)
  } else{
    add2_table(name, position)
  }
}


### 2 Tables
add2_table <- function(
  name,
  position
) {
  officer::ph_with_table_at(
    doc,
    value = name,
    left = dplyr::case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = dplyr::case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.25,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    ),
    height = dplyr::case_when(
      position == 'top' ~ 2.75,
      position == 'bottom' ~ 2.75,
      position == 'left' ~ 5,
      position == 'right' ~ 5
    ),
    width = dplyr::case_when(
      position == 'top' ~ 12,
      position == 'bottom' ~ 12,
      position == 'left' ~ 6,
      position == 'right' ~ 6
    )
  )
}


### 2 Flextables
add2_flextable <- function(
  name,
  position
) {
  flextable::ph_with_flextable_at(
    doc,
    value = name,
    left = dplyr::case_when(
      position == 'top' ~ .5,
      position == 'bottom' ~ .5,
      position == 'left' ~ .5,
      position == 'right' ~ 6.5
    ),
    top = dplyr::case_when(
      position == 'top' ~ 2,
      position == 'bottom' ~ 4.25,
      position == 'left' ~ 2,
      position == 'right' ~ 2
    )
  )
}

