### Description
### Description
#' Add 2 charts to PowerPoint slide
#'
#' This function adds 2 charts/tables/other objects to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R. For this function to work, you need a powerpoint object saved into R called "doc"
#' @param name The name of the chart/table/other object to be added to a new PowerPoint slide.
#' @param position Position options: "top"; "bottom"; "left"; "right". The chart layout can either be top-bottom OR left-right.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart table
#' @export
#' @examples
#' # First create a chart that you can add into a powerpoint object
#' my_table <- mtcars %>%
#'   y2clerk::freqs(carb) %>%
#'   orderlabel::order_label(inherent_order_label = T)
#'
#' # Then before adding additional slides, charts, or tables onto a powerpoint, you must first read a powerpoint into R
#' doc <- read_pptx('~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/Resources/Qualtrics Template New.pptx')
#'
#' # Now start adding in your charts
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
  position,
  label_first_only = F
) {
  if(label_first_only == F) {
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
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
    )
  } else{ #label_first_only == T
    officer::ph_with(
      doc,
      value = name,
      location = officer::ph_location(
        left = dplyr::case_when(
          position == 'top' ~ .5,
          position == 'bottom' ~ .5,
          position == 'left' ~ 0,
          position == 'right' ~ 8.25
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
          position == 'left' ~ 8.25,
          position == 'right' ~ 4.75
        )
      )
    )
  }
}
