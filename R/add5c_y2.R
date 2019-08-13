#### Add 5 Charts ####
#' Add 5 charts to PowerPoint slide
#'
#' This function adds 5 vertical charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "left"; "centerleft"; "center"; centerright"; "right". The chart layout is always 5 tall charts
#' @param label_first_only DEFAULT = F; Set to T if only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add5c_y2(chart_name, 'left')
#' doc <- add5c_y2(chart_name, 'centerleft')
#' doc <- add5c_y2(chart_name, 'center')
#' doc <- add5c_y2(chart_name, 'centerright')
#' doc <- add5c_y2(chart_name, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add5c_y2(chart_name, 'left', label_first_only = T)
#' doc <- add5c_y2(chart_name, 'centerleft', label_first_only = T)
#' doc <- add5c_y2(chart_name, 'center', T)
#' doc <- add5c_y2(chart_name, 'centerright', T)
#' doc <- add5c_y2(chart_name, 'right', T)


### Function
add5c_y2 <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F){
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'left' ~ -.125,
        position == 'centerleft' ~ 2.5,
        position == 'center' ~ 5.125,
        position == 'centerright' ~ 7.75,
        position == 'right' ~ 10.375
      ),
      top = dplyr::case_when(
        position == 'left' ~ 2,
        position == 'centerleft' ~ 2,
        position == 'center' ~ 2,
        position == 'centerright' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'left' ~ 5,
        position == 'centerleft' ~ 5,
        position == 'center' ~ 5,
        position == 'centerright' ~ 5,
        position == 'right' ~ 5
      ),
      width = dplyr::case_when(
        position == 'left' ~ 2.875,
        position == 'centerleft' ~ 2.875,
        position == 'center' ~ 2.875,
        position == 'centerright' ~ 2.875,
        position == 'right' ~ 2.875
      )
    ) } else{ #label_first == T
      mschart::ph_with_chart_at(
        doc,
        chart = name,
        left = dplyr::case_when(
          position == 'left' ~ .0,
          position == 'centerleft' ~ 4.4375,
          position == 'center' ~ 6.625,
          position == 'centerright' ~ 8.8125,
          position == 'right' ~ 11
        ),
        top = dplyr::case_when(
          position == 'left' ~ 2,
          position == 'centerleft' ~ 2,
          position == 'center' ~ 2,
          position == 'centerright' ~ 2,
          position == 'right' ~ 2
        ),
        height = dplyr::case_when(
          position == 'left' ~ 5,
          position == 'centerleft' ~ 5,
          position == 'center' ~ 5,
          position == 'centerright' ~ 5,
          position == 'right' ~ 5
        ),
        width = dplyr::case_when(
          position == 'left' ~ 4.5,
          position == 'centerleft' ~ 2.25,
          position == 'center' ~ 2.25,
          position == 'centerright' ~ 2.25,
          position == 'right' ~ 2.25
        )
      )
    }
}

