#' String Wrapping Function (Y2 Private)
#'
#' Modified string wrapping helper function that does not over-write linebreaks by default (see also the base R `strwrap()` function)
#'
#' @param x a character vector, or an object which can be converted to a character vector by `as.character()`.
#' @param width a positive integer giving the target column for wrapping lines in the output.
#' @param overwrite_breaks DEFAULT: FALSE; Whether the function overwrites existing linebreaks in an input string.
#' @param indent a non-negative integer giving the indentation of the first line in a paragraph.
#' @param exdent a non-negative integer specifying the indentation of subsequent lines in paragraphs.
#' @param prefix,initial a character string to be used as prefix for each line except the first, for which `initial` is used.
#' @param simplify a logical. If TRUE, the result is a single character vector of line text; otherwise, it is a list of the same length as x the elements of which are character vectors of line text obtained from the corresponding element of x. (Hence, the result in the former case is obtained by unlisting that of the latter.)
#' @keywords internal

strwrap_helper <- function (
    x,
    width = width,
    overwrite_breaks = FALSE,
    indent = 0,
    exdent = 0,
    prefix = "",
    simplify = simplify,
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
