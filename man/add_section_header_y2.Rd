% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_section_header_y2.R
\name{add_section_header_y2}
\alias{add_section_header_y2}
\title{Add Blank PowerPoint Section Header}
\usage{
add_section_header_y2(
  title = "Title",
  n = "1",
  report_style = c("qualtrics", "municipal", "y2"),
  master_name = NULL,
  slide_name = NULL,
  text_boxes = TRUE
)
}
\arguments{
\item{title}{DEFAULT: "Title"; Add a title in quotes here to have your Section Headers put in for you by R}

\item{n}{DEFAULT: "1"; The number added to your section header (for Qualtrics style reports only). If section 2, put "2", etc.}

\item{report_style}{DEFAULT: "qualtrics"; The report style/template you are using -- must be either 'qualtrics', 'municipal', or 'y2'}

\item{master_name}{DEFAULT: NULL; The name of the PP master layout that the slide_name comes from. If no argument provided and the report style is 'qualtrics', defaults to "1_Office Theme"; if no argument provided and the report style is 'municipal' or 'y2', defaults to "Office Theme".}

\item{slide_name}{DEFAULT: "Section Header; The name of the type of the PP slide you want added to the PP}

\item{text_boxes}{DEFAULT = TRUE; Automatically adds a title and section number. Set to FALSE for no text boxes}
}
\description{
This function adds a new PowerPoint section header along with a title box
}
\examples{
# Before adding additional slides, charts, or tables onto a powerpoint,
# you must first read a powerpoint into R
\dontrun{
doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
doc <- add_section_header_y2()
print(doc, '~/Desktop/test.pptx')
}
}
\keyword{powerpoint}
\keyword{slide}
