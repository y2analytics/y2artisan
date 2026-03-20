# Add Blank PowerPoint Section Header

This function adds a new PowerPoint section header along with a title
box

## Usage

``` r
add_section_header_y2(
  title = "Title",
  n = "1",
  report_style = c("qualtrics", "municipal", "y2"),
  master_name = NULL,
  slide_name = NULL,
  text_boxes = TRUE
)
```

## Arguments

- title:

  DEFAULT: "Title"; Add a title in quotes here to have your Section
  Headers put in for you by R

- n:

  DEFAULT: "1"; The number added to your section header (for Qualtrics
  style reports only). If section 2, put "2", etc.

- report_style:

  DEFAULT: "qualtrics"; The report style/template you are using – must
  be either 'qualtrics', 'municipal', or 'y2'

- master_name:

  DEFAULT: NULL; The name of the PP master layout that the slide_name
  comes from. If no argument provided and the report style is
  'qualtrics', defaults to "1_Office Theme"; if no argument provided and
  the report style is 'municipal' or 'y2', defaults to "Office Theme".

- slide_name:

  DEFAULT: "Section Header; The name of the type of the PP slide you
  want added to the PP

- text_boxes:

  DEFAULT = TRUE; Automatically adds a title and section number. Set to
  FALSE for no text boxes

## Examples

``` r
# Before adding additional slides, charts, or tables onto a powerpoint,
# you must first read a powerpoint into R
if (FALSE) { # \dontrun{
doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
Qualtrics/2021 Template and Resources/Template for mscharts.pptx')
doc <- add_section_header_y2()
print(doc, '~/Desktop/test.pptx')
} # }
```
