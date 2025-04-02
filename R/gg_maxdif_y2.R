#### gg_maxdif_y2 ####
### Description
#' Create a grouped ggplot object: UNDER CONSTRUCTION
#'
#' This function creates a ggplot2 object automatically formatted for a maxdiff type variable with a positive and negative side.
#' @param data DEFAULT = frequencies; The name of the data frame that ggplot pulls from.
#' @param x_var DEFAULT = label; When using the freqs function, will typically be label (is by default).
#' @param y_var DEFAULT = result; When using the freqs function, will typically be result (is by default).
#' @param label_var DEFAULT = percent_label; When using the order_label function, this variable will be created for you.
#' @param color_var DEFAULT = group_var. Note: the color variable CANNOT be numeric.
#' @param axis_text_size DEFAULT = 12; Font size for variable levels and axis percentages.
#' @param axis_title_size DEFAULT = 14; Font size for x_label and y_label.
#' @param bar_width DEFAULT = .75, with a bar_width of 1 meaning each bars touches the ones next to it
#' @param chart_height DEFAULT = 5.5, If saving out a vertical bar chart with a different height, set the height here to have the nudge argument adjust itself automatically
#' @param chart_width DEFAULT = 11, If saving out a horizontal bar chart with a different width, set the width here to have the nudge argument adjust itself automatically
#' @param fills NO DEFAULT; requires a vector of colors for all levels of the color_var/grouping variable
#' @param font_family DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions
#' @param label_length DEFAULT = 45 for horizontal charts and 15 for vertical charts. This determines how many characters an x-axis label can be before R inserts a line break.
#' @param label_size DEFAULT = 6. Adjusts the size of the percent labels over each bar.
#' @param legend_nrow DEFAULT = NULL; Change to a numeric to specify the number of rows for the legend
#' @param legend_pos DEFAULT = 'top'
#' @param legend_rev DEFAULY = FALSE
#' @param legend_text_size DEFAULT = 8
#' @param legend_title_size DEFAULT = 8
#' @param legend_title DEFAULT = '', If you put in a title, the legend will default to 'top' unless otherwise specified
#' @param nudge DEFAULT = 0; however, nudge automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @param title_label DEFAULT = ''; Add your title in "" as the title of the chart.
#' @param title_size DEFAULT = 18
#' @param x_label,y_label DEFAULT = ''; Title for the x_axis or y_axis
#' @param y_min DEFAULT = 0 to show full data without skewing perspective, but can be adjusted.
#' @param y_max DEFAULT = 0; however, the y_max automatically adjusts based on the max value of 'result', in most cases fitting the chart perfectly
#' @keywords chart ggplot bar single
#' @export
#' @examples
#' # Function still in development


gg_maxdif_y2 <- function(
  data = frequencies,
  x_var = label,
  y_var = result,
  label_var = percent_label,
  color_var = group_var,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  chart_height = 5.5,
  chart_width = 11,
  fills, # only variable with no default...
  font_family = 'Flama',
  label_length = 45,
  label_size = 6,
  legend_pos = 'top',
  legend_nrow = NULL,
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = '',
  nudge = 0, # auto-fills
  title_label = '',
  title_size = 14,
  x_label = '',
  y_label = '',
  y_min = 0, # auto-fills
  y_max = 0 # auto-fills
) {

### Check fonts
  if (
    (stringr::str_detect(sysfonts::font_families(), font_family) %>% sum == 0)
  ) {
    stop("The font you specified in the 'font_family' argument does not exist in your R session")
  }


### Flags
  label <- result <- percent_label <- group_var <- NULL
  x_flag <- dplyr::enquo(x_var)
  y_flag <- dplyr::enquo(y_var)
  color_flag <- dplyr::enquo(color_var) # AKA group_var
  label_flag <- dplyr::enquo(label_var)



### Set defaults
  max_y_val <- data %>% dplyr::summarise(max(!!y_flag)) %>% as.numeric()
  min_y_val <- data %>% dplyr::summarise(min(!!y_flag)) %>% as.numeric()
  max_str_length <- data %>%
    dplyr::select(!!x_flag) %>%
    purrr::as_vector() %>%
    stringr::str_length() %>%
    max()
  str_add <- max_str_length * max_y_val / 1500
  y_max <- dplyr::case_when(
    y_max != 0 ~ y_max,
    chart_width < 11 ~  (max_y_val + (max_y_val / chart_width) * 2),
    TRUE ~  (max_y_val + max_y_val / 5)
  )
  y_min <- dplyr::case_when(
    y_min != 0 ~ y_min,
    chart_width < 11 ~  (min_y_val - (min_y_val / chart_width) * 2),
    TRUE ~  (min_y_val + min_y_val / 5)
  )
  nudge_x <- dplyr::case_when(
    nudge != 0 ~ nudge,
    TRUE ~ (max_y_val / (max_y_val * 4) + str_add) * -1
  )
  label_length <- dplyr::case_when(
    label_length != 45 ~ label_length,
    TRUE ~ label_length
  )



### Chart
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!x_flag,
      y = !!y_flag,
      fill = !!color_flag
    )
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(
        fill = !!color_flag
      ),
      stat = 'identity',
      width = bar_width
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = !!label_flag,
        color = !!color_flag
      ),
      family = font_family,
      size = label_size,
      hjust = ifelse(data$result > 0, nudge_x, -nudge_x * 5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(
        reverse = legend_rev,
        nrow = legend_nrow
      ),
      values = fills
    ) +
    ggplot2::scale_color_manual(
      guide = 'none',
      values = fills
    ) +
    ggplot2::ggtitle(
      title_label
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      legend.position = legend_pos,
      legend.spacing.x = ggplot2::unit(2, 'mm'),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = title_size),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      labels = function(x) stringr::str_c((round(x, 2)) * 100, '%')
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) lapply(
        strwrap_mod(
          x,
          width = label_length,
          simplify = FALSE
        ),
        paste,
        collapse="\n"
      )
    ) + ggplot2::coord_flip()
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
