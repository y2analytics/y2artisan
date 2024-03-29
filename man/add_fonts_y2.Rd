% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_fonts_y2.R
\name{add_fonts_y2}
\alias{add_fonts_y2}
\title{Adds fonts and font families to your R session}
\usage{
add_fonts_y2(show_fonts_added = FALSE)
}
\arguments{
\item{show_fonts_added}{DEFAULT = FALSE. If TRUE, shows the available fonts loaded in by add_fonts_y2().}
}
\value{
A message
}
\description{
Use add_fonts_y2() to add the most commonly used fonts at Y2 to your Rsession. This will make them easily available for charting in ggplot and other instances where fonts may be needed. Use list_fonts_y2() to see which fonts will be loaded to your session.
}
\examples{
add_fonts_y2()
}
\keyword{font}
\keyword{showtext}
