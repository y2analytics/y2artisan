#### Add 4 Charts ####
### Description
#' Add 4 charts to PowerPoint slide
#'
#' This function adds 4 charts to a PowerPoint slide. The charts are automatically added to the last slide of the PP object in R.
#' @param name The name of the ms_chart object to be added to a new PowerPoint slide.
#' @param position Position options: "topright"; "bottomright"; "topleft"; "bottomleft"; "left"; "centerleft"; "centerright"; "right". The chart layout can either be left-centerleft-centerright-right OR topleft-bottomleft-topright-bottomright.
#' @param label_first_only DEFAULT = F; Set to T if for a series of left-to-right charts where only the first chart has axis labels. Changing this setting to T in this case will slightly adjut positioning for equally sized graphs
#' @keywords chart
#' @export
#' @examples
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c_y2(chart_name, 'topright')
#' doc <- add4c_y2(chart_name, 'bottomright')
#' doc <- add4c_y2(chart_name, 'topleft')
#' doc <- add4c_y2(chart_name, 'bottomleft')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c_y2(chart_name, 'left')
#' doc <- add4c_y2(chart_name, 'centerleft')
#' doc <- add4c_y2(chart_name, 'centerright')
#' doc <- add4c_y2(chart_name, 'right')
#' OR
#' doc <- officer::add_slide(doc, layout = "Findings / 1 chart", master = "Office Theme")
#' doc <- add4c_y2(chart_name, 'left', label_first_only = T)
#' doc <- add4c_y2(chart_name, 'centerleft', label_first_only = T)
#' doc <- add4c_y2(chart_name, 'centerright', T)
#' doc <- add4c_y2(chart_name, 'right', T)


### Function
add4c_y2 <- function(
  name,
  position,
  label_first_only = F
) {
  if(label_first_only == F){
    mschart::ph_with_chart_at(
      doc,
      chart = name,
      left = dplyr::case_when(
        position == 'topright' ~ 6.5,
        position == 'bottomright' ~ 6.5,
        position == 'topleft' ~ .5,
        position == 'bottomleft' ~ .5,
        position == 'left' ~ .0,
        position == 'centerleft' ~ 3.25,
        position == 'centerright' ~ 6.5,
        position == 'right' ~ 9.75
      ),
      top = dplyr::case_when(
        position == 'topright' ~ 2,
        position == 'bottomright' ~ 4.25,
        position == 'topleft' ~ 2,
        position == 'bottomleft' ~ 4.25,
        position == 'left' ~ 2,
        position == 'centerleft' ~ 2,
        position == 'centerright' ~ 2,
        position == 'right' ~ 2
      ),
      height = dplyr::case_when(
        position == 'topright' ~ 2.75,
        position == 'bottomright' ~ 2.75,
        position == 'topleft' ~ 2.75,
        position == 'bottomleft' ~ 2.75,
        position == 'left' ~ 5,
        position == 'centerleft' ~ 5,
        position == 'centerright' ~ 5,
        position == 'right' ~ 5
      ),
      width = dplyr::case_when(
        position == 'topright' ~ 6,
        position == 'bottomright' ~ 6,
        position == 'topleft' ~ 6,
        position == 'bottomleft' ~ 6,
        position == 'left' ~ 3.5,
        position == 'centerleft' ~ 3.5,
        position == 'centerright' ~ 3.5,
        position == 'right' ~ 3.5
      )
    ) } else{ #label_first == T
      mschart::ph_with_chart_at(
        doc,
        chart = name,
        left = dplyr::case_when(
          position == 'topright' ~ 8.25,
          position == 'bottomright' ~ 8.25,
          position == 'topleft' ~ 0,
          position == 'bottomleft' ~ 0,
          position == 'left' ~ .0,
          position == 'centerleft' ~ 4.75,
          position == 'centerright' ~ 7.5,
          position == 'right' ~ 10.25
        ),
        top = dplyr::case_when(
          position == 'topright' ~ 2,
          position == 'bottomright' ~ 4.25,
          position == 'topleft' ~ 2,
          position == 'bottomleft' ~ 4.25,
          position == 'left' ~ 2,
          position == 'centerleft' ~ 2,
          position == 'centerright' ~ 2,
          position == 'right' ~ 2
        ),
        height = dplyr::case_when(
          position == 'topright' ~ 2.75,
          position == 'bottomright' ~ 2.75,
          position == 'topleft' ~ 2.75,
          position == 'bottomleft' ~ 2.75,
          position == 'left' ~ 5,
          position == 'centerleft' ~ 5,
          position == 'centerright' ~ 5,
          position == 'right' ~ 5
        ),
        width = dplyr::case_when(
          position == 'topright' ~ 4.75,
          position == 'bottomright' ~ 4.75,
          position == 'topleft' ~ 8.25,
          position == 'bottomleft' ~ 8.25,
          position == 'left' ~ 5,
          position == 'centerleft' ~ 3,
          position == 'centerright' ~ 3,
          position == 'right' ~ 3
        )
      )
    }
}
