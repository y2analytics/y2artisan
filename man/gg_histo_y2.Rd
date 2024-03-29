% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_histo_y2.R
\name{gg_histo_y2}
\alias{gg_histo_y2}
\title{Create a ggplot2 histogram}
\usage{
gg_histo_y2(
  data,
  x_var,
  fills = "#474E7E",
  axis_text_size = 16,
  axis_title_size = 18,
  bins = NULL,
  binwidth = NULL,
  color_mean_line = "#474747",
  font_family = "Flama",
  mean_line = c("mean", "median", "none"),
  quadrant_lines = FALSE,
  weight_var = NULL,
  x_limits = "no limits",
  x_label = "",
  y_label = "Respondents"
)
}
\arguments{
\item{data}{NO DEFAULT; a data frame containing the variable you want to histogram}

\item{x_var}{NO DEFAULT; the variable name for which you want to create a histogram}

\item{fills}{NO DEFAULT; the fill color for the histogram}

\item{axis_text_size}{DEFAULT = 16; Font size for scale points along the axis}

\item{axis_title_size}{DEFAULT = 18; Font size for x_label and y_label}

\item{bins}{DEFAULT = 30; The number of bins. Same as the ggplot2 function, geom_histogram}

\item{binwidth}{DEFAULT = NULL; The number of units in the x_var that fit in a bin. Overrides the bins argument}

\item{color_mean_line}{DEFAULT = '#474747', a gray/black color}

\item{font_family}{DEFAULT = 'Flama'; all fonts used need to be previously loaded in using the font_add() and showtext_auto() functions}

\item{mean_line}{DEFAULT = "mean", must be one of c("mean", "median", "none")}

\item{quadrant_lines}{DEFAULT = FALSE. Set to TRUE to display dotted lines on the 25th and 75th percentiles}

\item{weight_var}{DEFAULT = NULL; set to your weights variable if working with weighted data}

\item{x_limits}{DEFAULT = 'no limits'; The 'no limits' default allows the histogram to capture all values for the variable. A secondary option is '95 trim' which will set the limits of the histogram to within 2 standard deviations of the mean, or all values between the 5th and 95th percentiles. Alternatively, you can set your own limits using c(my_min, my_max)}

\item{x_label}{DEFAULT = ''; Title for the x_axis}

\item{y_label}{DEFAULT = 'Respondents'; Title for the x_axis (the y axis is set to show the number of respondents in a given bin)}
}
\description{
This function creates a ggplot2 histogram object of a given variable with a dotted vertical line at the mean
}
\examples{
chart <- iris \%>\% gg_histo_y2(
  Petal.Width,
  'purple',
  binwidth = .25,
  font_family = 'sans'
)
}
\keyword{bar}
\keyword{chart}
\keyword{ggplot}
\keyword{single}
