% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wordcloud_mono_y2.R
\name{wordcloud_mono_y2}
\alias{wordcloud_mono_y2}
\title{Creates a word cloud from an open end question}
\usage{
wordcloud_mono_y2(
  dataset,
  variable,
  colors = "#474E7E",
  max_size = 12,
  min_size = 1,
  font_family = "Flama",
  top_x = 50
)
}
\arguments{
\item{dataset}{no default. Usually piped in from your main dataset}

\item{variable}{The name of the openended variable from your dataset you want to look at}

\item{colors}{DEFAULT = '#474E7E' (bluepurple from Qualtrics template). All words are the same color. Any color may be specified as a hexcode}

\item{max_size}{DEFAULT = 12; the largest text size for the word with the highest frequency#' @keywords openend open end wordcloud word cloud}

\item{min_size}{DEFAULT = 1; the smallest text size for the word with the lowest frequency}

\item{font_family}{DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions}

\item{top_x}{DEFAULT = 50; Shows the top X most commonly mentioned words you want to see from the open-end}
}
\description{
Finds the frequencies of each word in an open end question and creates a word cloud based on the frequencies. Words mentioned less are smaller
}
\examples{
responses <- tibble::tibble(
  var1 = c(
    'I like to talk about dogs',
    'Dogs are cool but cats are aight too',
    'I prefer dogs over cats',
    "My dog's collars are always too tight",
    'One last sentence about dogs',
    'Cats collars are typically cooler than dogs'
  )
)

responses \%>\% wordcloud_mono_y2(var1, font_family = "Arial")
responses \%>\% wordcloud_mono_y2(var1, 'red', font_family = "Arial")
}
