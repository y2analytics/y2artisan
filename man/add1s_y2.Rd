% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add1s_y2.R
\name{add1s_y2}
\alias{add1s_y2}
\title{Add 1 Blank PowerPoint slide}
\usage{
add1s_y2(
  title = "Title",
  commentary = "Commentary",
  footer = "Footer",
  report_style = c("qualtrics", "municipal", "y2"),
  text_boxes = TRUE,
  slide_name = "Blank",
  master_name = "1_Office Theme",
  title_color = NULL,
  commentary_color = NULL,
  footer_color = NULL,
  font_title = NULL,
  font_text = NULL,
  title_bg_color = "#1A497A",
  commentary_bg_color = "#9EBCDB",
  footer_left = NULL,
  footer_top = NULL,
  footer_width = NULL,
  footer_height = NULL
)
}
\arguments{
\item{title}{DEFAULT: 'Title'; Add a slide title in quotes, automatically formatted to the specified report style template}

\item{commentary}{DEFAULT: 'Commentary'; Add commentary/desctiption in quotes, automatically formatted to the specified report style template}

\item{footer}{DEFAULT: 'Footer'; Add a footer in quotes}

\item{report_style}{DEFAULT: "qualtrics"; The report style/template you are using -- must be either 'qualtrics', 'municipal', or 'y2'}

\item{text_boxes}{DEFAULT = TRUE; Automatically adds title, commentary, and footer boxes to the blank slide. Set to F for no text boxes}

\item{slide_name}{DEFAULT: 'Blank'; The name of the type of the PP slide you want added to the PP}

\item{master_name}{DEFAULT: '1_Office Theme'; The name of the PP master layout that the slide_name comes from}

\item{title_color}{DEFAULT: NULL; Color or hexcode for slide title. If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports}

\item{commentary_color}{DEFAULT: NULL; Color or hexcode for slide commentary If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports}

\item{footer_color}{DEFAULT: NULL; Color or hexcode for slide footer. If no value provided, will be set to black for Qualtrics style reports and white for Municipal style reports}

\item{font_title}{DEFAULT: NULL; font for slide title. If no value provided, will be set to 'BentonSans Regular' for Qualtrics style reports and 'Flama Medium' for Municipal style reports}

\item{font_text}{DEFAULT: NULL; font for slide text boxes and footer. If no value provided, will be set to 'BentonSans Regular' for Qualtrics style reports and 'Flama Light' for Municipal style reports}

\item{title_bg_color}{DEFAULT: '#1A497A; Background color of slide title text box (only used for Municipal style reports)}

\item{commentary_bg_color}{DEFAULT: '#9EBCDB; Background color of slide commentary text box (only used for Municipal style reports)}

\item{footer_left}{DEFAULT = NULL; how far footer will start from left of slide. If no value provided, defaults to approximately 0.35 for either report style}

\item{footer_top}{DEFAULT = NULL; how far footer will start from top of slide. If no value provided, defaults to approximately 7 for either report style}

\item{footer_width}{DEFAULT = NULL; height of footer on slide. If no value provided, defaults to approximately 11.5 for either report style}

\item{footer_height}{DEFAULT = NULL; width of footer on slide. If no value provided, defaults to approximately 0.5 for either report style}
}
\description{
This function adds a new PowerPoint slide. For this function to work, you need a powerpoint object saved into R called 'doc'
}
\examples{

# Before adding slides, charts, or tables onto a powerpoint,
# you must first read a PowerPoint into R
\dontrun{
doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
Qualtrics/2021 Template and Resources/Template for mscharts.pptx')

# Now start adding in your charts
doc <- add1s_y2()

print(doc, '~/Desktop/test.pptx')
}
}
\keyword{powerpoint}
\keyword{slide}
