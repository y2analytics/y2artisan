#### gg_pie_y2 ####
### Description
#' Create a pie chart ggplot object
#'
#' This function creates a ggplot2 object automatically formatted for a pie chart.
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = label; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param colors DEFAULT is white ('#ffffff') for the text of all percent labels; You may also 1) Specify 1 color, and this color will be applied to all color_var levels or 2) Specify a vector of colors for each individual level of the color_var
#' @param fills NO DEFAULT; requires a vector of colors for all levels of the x_var
#' @param font_family DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param label_length DEFAULT = 15; This determines how many characters a label on a pie slice can be before R inserts a line break.
#' @param label_size DEFAULT = 10. Adjusts the size of the percent labels over each bar.
#' @param legend_pos DEFAULT = 'none'
#' @param legend_rev DEFAULT = FALSE
#' @param legend_text_size DEFAULT = 8
#' @param overwrite_breaks DEFAULT = TRUE, Whether to overwrite existing linebreaks ("\n") in string label inputs when performing text pre-processing, such as string wrapping
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' frequencies <- iris %>% y2clerk::freqs(Species)
#'
#' chart <- gg_pie_y2(
#'   fills = c('red', 'blue', 'pink'),
#'   font_family = 'sans'
#' )


gg_pie_y2 <- function(
  data = frequencies,
  x_var = label,
  y_var = result,
  colors = '#ffffff',
  fills, #only variable with no default...
  font_family = 'Flama',
  label_length = 15,
  label_size = 10,
  legend_pos = 'none',
  legend_rev = FALSE,
  legend_text_size = 8,
  overwrite_breaks = TRUE,
  title_label = '',
  title_size = 14
) {

  ### Check fonts
  if (
    (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
  ) {
    stop("The font you specified in the 'font_family' argument does not exist in your R session")
  }


  ### Flags
  label <- result <- percent_label <- NULL
  x_flag <- dplyr::enquo(x_var)
  y_flag <- dplyr::enquo(y_var)
  color_flag <- dplyr::enquo(x_var)


  ### Set defaults
  colors <- if (length(colors) == 1) { # If user specifies only one color, repeat color for all bars
    colors <- rep(colors, dplyr::count(data))
  } else {
    colors <- colors
  }

  data <- data %>%
    dplyr::mutate(
      percent_label = stringr::str_c(
        !!x_flag,
        '\n',
        !!y_flag * 100,
        '%'
      )
    )


  ### Conditional chunks
if (legend_pos != 'none') {
  data <- data %>%
    dplyr::mutate(
      percent_label = stringr::str_c(
        !!y_flag * 100,
        '%'
      )
    )
} else (data <- data)


  ### Chart
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = "",
      y = !!y_flag,
      fill = !!x_flag
    )
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(
        fill = !!color_flag
      ),
      width = 1,
      stat = 'identity',
      position = "fill"
    ) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = percent_label,
        color = !!color_flag
      ),
      family = font_family,
      size = label_size,
      position = ggplot2::position_fill(vjust = 0.5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(reverse = legend_rev),
      values = fills
    ) +
    ggplot2::scale_color_manual(
      guide = 'none',
      values = colors
    ) +
    ggplot2::ggtitle(
      title_label
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      fill = ""
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = title_size),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) lapply(
        strwrap_mod(
          x,
          width = label_length,
          overwrite_breaks = overwrite_breaks,
          simplify = FALSE
        ),
        paste,
        collapse="\n"
      )
    )
}

# Private functions -------------------------------------------------------

strwrap_mod <- function (
    x, 
    width = 0.9 * getOption("width"),
    overwrite_breaks = overwrite_breaks,
    indent = 0, 
    exdent = 0, 
    prefix = "", 
    simplify = TRUE, 
    initial = prefix
) {
  
  if (!is.character(x)) 
    x <- as.character(x)
  indentString <- strrep(" ", indent)
  exdentString <- strrep(" ", exdent)
  y <- list()
  enc <- Encoding(x)
  x <- enc2utf8(x)
  if (any(ind <- !validEnc(x))) 
    x[ind] <- iconv(x[ind], "UTF-8", "UTF-8", sub = "byte")
  
  if (overwrite_breaks) {
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE), strsplit, 
                "[ \t\n]", perl = TRUE)
  } else {
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE), strsplit, 
                "[ \t]", perl = TRUE)
  }
  
  for (i in seq_along(z)) {
    yi <- character()
    for (j in seq_along(z[[i]])) {
      words <- z[[i]][[j]]
      nc <- nchar(words, type = "w")
      if (anyNA(nc)) {
        nc0 <- nchar(words, type = "b")
        nc[is.na(nc)] <- nc0[is.na(nc)]
      }
      if (any(nc == 0L)) {
        zLenInd <- which(nc == 0L)
        zLenInd <- zLenInd[!(zLenInd %in% (grep("[.?!][)\"']{0,1}$", 
                                                words, perl = TRUE, useBytes = TRUE) + 1L))]
        if (length(zLenInd)) {
          words <- words[-zLenInd]
          nc <- nc[-zLenInd]
        }
      }
      if (!length(words)) {
        yi <- c(yi, "", initial)
        next
      }
      currentIndex <- 0L
      lowerBlockIndex <- 1L
      upperBlockIndex <- integer()
      lens <- cumsum(nc + 1L)
      first <- TRUE
      maxLength <- width - nchar(initial, type = "w") - 
        indent
      while (length(lens)) {
        k <- max(sum(lens <= maxLength), 1L)
        if (first) {
          first <- FALSE
          maxLength <- width - nchar(prefix, type = "w") - 
            exdent
        }
        currentIndex <- currentIndex + k
        if (nc[currentIndex] == 0L) 
          upperBlockIndex <- c(upperBlockIndex, currentIndex - 
                                 1L)
        else upperBlockIndex <- c(upperBlockIndex, currentIndex)
        if (length(lens) > k) {
          if (nc[currentIndex + 1L] == 0L) {
            currentIndex <- currentIndex + 1L
            k <- k + 1L
          }
          lowerBlockIndex <- c(lowerBlockIndex, currentIndex + 
                                 1L)
        }
        if (length(lens) > k) 
          lens <- lens[-seq_len(k)] - lens[k]
        else lens <- NULL
      }
      nBlocks <- length(upperBlockIndex)
      s <- paste0(c(initial, rep.int(prefix, nBlocks - 
                                       1L)), c(indentString, rep.int(exdentString, nBlocks - 
                                                                       1L)))
      initial <- prefix
      for (k in seq_len(nBlocks)) s[k] <- paste0(s[k], 
                                                 paste(words[lowerBlockIndex[k]:upperBlockIndex[k]], 
                                                       collapse = " "))
      yi <- c(yi, s, prefix)
    }
    y <- if (length(yi)) 
      c(y, list(yi[-length(yi)]))
    else c(y, "")
  }
  if (length(pos <- which(enc == "latin1"))) {
    y[pos] <- lapply(y[pos], function(s) {
      e <- Encoding(s)
      if (length(p <- which(e == "UTF-8"))) 
        s[p] <- iconv(s[p], "UTF-8", "latin1", sub = "byte")
      s
    })
  }
  if (simplify) 
    y <- as.character(unlist(y))
  y
  
}
