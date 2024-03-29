% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_y2.R
\name{theme_y2}
\alias{theme_y2}
\title{Add the classic y2 theme to any ggplot2 chart}
\usage{
theme_y2(font_family = "Flama")
}
\arguments{
\item{font_family}{DEFAULT = 'Flama'}
}
\description{
All y2artisan charts already have the y2 theme built in, but if you're creating a more custom ggplot2 chart and want to add the classic y2 theme, use theme_y2()
}
\examples{
\dontrun{
frequencies <- mtcars \%>\%
  y2clerk::freqs(carb) \%>\%
  orderlabel::order_label(inherent_order_label = TRUE)

chart <- ggplot2::ggplot(
  frequencies,
  ggplot2::aes(x = label, y = result)
) +
  ggplot2::geom_bar(stat = 'identity') +
  theme_y2()
  }
}
\keyword{chart}
\keyword{ggplot}
\keyword{theme}
