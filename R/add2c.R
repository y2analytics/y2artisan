#### Add 2 Charts ####
### Description
#' Add 2 charts to PowerPoint slide
#'
#' This function adds 2 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "top"; "bottom"; "left"; "right". The chart layout can either be top-bottom OR left-right.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'left')
#' doc <- add2c(chart_name, 'right')
#' OR
#' #' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'left', label_first_only = T)
#' doc <- add2c(chart_name, 'right', T)
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add2c(chart_name, 'top')
#' doc <- add2c(chart_name, 'bottom')

### Function
add2c <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F) {
    mschart::ph_with_chart_at(
      doc,
      chart = name,
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
  } else{ #label_first_only == T
    mschart::ph_with_chart_at(
      doc,
      chart = name,
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
  }
}

