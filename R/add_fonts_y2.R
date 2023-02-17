# Public function ---------------------------------------------------------
### add_fonts_y2

#' Adds fonts and font families to your R session
#'
#' Use add_fonts_y2() to add the most commonly used fonts at Y2 to your Rsession. This will make them easily available for charting in ggplot and other instances where fonts may be needed. Use list_fonts_y2() to see which fonts will be loaded to your session.
#'
#' @keywords showtext font
#' @param show_fonts_added DEFAULT = FALSE. If TRUE, shows the available fonts loaded in by add_fonts_y2().
#' @export
#' @return A message
#' @examples
#' add_fonts_y2()

add_fonts_y2 <- function(
    show_fonts_added = FALSE
  ) {

  fonts <- list_fonts_y2()

  for (i in 1:length(fonts$font_family)) {

    possibleError <- tryCatch(

      sysfonts::font_add(
          family = fonts$font_family[i],
          regular = fonts$font_regular[i],
          bold = fonts$font_bold[i],
          italic = fonts$font_italic[i],
          bolditalic = fonts$font_bold_italic[i]
        )
      ,
      error=function(e) print(
        stringr::str_c(
          'A font file in font family ',
          fonts$font_family[i],
          ' not found. Download and install if you need this font.'
        )
      )
    )

    if(inherits(possibleError, "error")) next

  }

  showtext::showtext_auto()

  if (show_fonts_added == TRUE) {
    print(fonts)
  }

  print('All available fonts loaded into Rsession')

}

