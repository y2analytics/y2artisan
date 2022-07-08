# Public function ---------------------------------------------------------
### list_fonts_y2

#' Lists the fonts loaded in by add_fonts_y2
#'
#' Use list_fonts_y2() to see the available fonts loaded in by add_fonts_y2().
#'
#' @keywords showtext font
#' @export
#' @return A dataframe
#' @examples
#' list_fonts_y2()

list_fonts_y2 <- function(){

  tidyr::tibble(
    font_family =
      c(
        'BentonSans',
        'Flama',
        'Roboto',
        'Pokemon',
        'Arial',
        'FreightSans',
        'Neutraface',
        'Proxima Nova',
        'Trade Gothic',
        'Century Gothic',
        'Circular',
        'Facebook Reader',
        'Lato',
        'Optimist',
        'Proxima Nova',
        'Segoe UI',
        'Montserrat',
        'Gill Sans'
      ),
    font_regular =
      c(
        'BentonSans Regular.otf',
        'Flama-Medium.otf',
        'Roboto-Regular.ttf',
        'PokemonHollow.ttf',
        'Arial.ttf',
        'FreigSanProMed.otf',
        'NeutraText-Book.otf',
        'Proxima Nova Regular.otf',
        'Linotype - TradeGothicLTPro-Cn18.otf',
        'CenturyGothic.ttf',
        'circular-medium.otf',
        'Facebook Reader Medium.ttf',
        'Lato-Medium.ttf',
        'Optimist Normal.ttf',
        'Proxima Nova Regular.otf',
        'segoeui.ttf',
        'Montserrat-Regular.ttf',
        'GillSans.ttc'
      ),
    font_bold =
      c(
        'BentonSans Bold.otf',
        'Flama-Bold.otf',
        'Roboto-Bold.ttf',
        'PokemonHollow.ttf',
        'Arial Bold.ttf',
        'FreigSanProBold.otf',
        'NeutraText-Bold.otf',
        'Proxima Nova Bold.otf',
        'Linotype - TradeGothicLTPro-BdCn20.otf',
        'GOTHICB0.TTF',
        'circular-bold.otf',
        'Facebook Reader Medium.ttf',
        'Lato-Bold.ttf',
        'Optimist Normal.ttf',
        'Proxima Nova Bold.otf',
        'segoeuib.ttf',
        'Montserrat-Bold.ttf',
        'GillSans.ttc'

      ),
    font_italic =
      c(
        'BentonSans Regular.otf',
        'Flama-MediumItal.otf',
        'Roboto-Italic.ttf',
        'PokemonHollow.ttf',
        'Arial Italic.ttf',
        'FreigSanProMedIt.otf',
        'NeutraText-BookItalic.otf',
        'Proxima Nova Regular.otf',
        'Linotype - TradeGothicLTPro-Cn18Obl.otf',
        'GOTHICI.TTF',
        'circular-medium-italic.otf',
        'Facebook Reader Medium.ttf',
        'Lato-MediumItalic.ttf',
        'Optimist Normal.ttf',
        'Proxima Nova Regular.otf',
        'segoeui.ttf',
        'Montserrat-Italic.ttf',
        'GillSans.ttc'

      ),
    font_bold_italic =
      c(
        'BentonSans Regular.otf',
        'Flama-BoldItalic.otf',
        'Roboto-BoldItalic.ttf',
        'PokemonHollow.ttf',
        'Arial Bold Italic.ttf',
        'FreigSanProBoldIt.otf',
        'NeutraText-BoldItalic.otf',
        'Proxima Nova Bold.otf',
        'Linotype - TradeGothicLTPro-BdCn20.otf',
        'GOTHICBI.TTF',
        'circular-bold-italic.otf',
        'Facebook Reader Medium.ttf',
        'Lato-BoldItalic.ttf',
        'Optimist Normal.ttf',
        'Proxima Nova Bold.otf',
        'segoeuib.ttf',
        'Montserrat-BoldItalic.ttf',
        'GillSans.ttc'

      )
  )

}
