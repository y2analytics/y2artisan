# Add a Custom PPT Table Legend

This function adds a custom legend of specified labels and color palette
to the last slide in your report. For this function to work, you need a
powerpoint object saved into R called 'doc'

## Usage

``` r
add_legend_y2(
  labels,
  palette,
  font_family = "Flama Medium",
  font_size = 12,
  label_color = "white",
  position = "center"
)
```

## Arguments

- labels:

  A vector of labels to use to create your legend

- palette:

  A color palette to use as the keys for your legend

- font_family:

  DEFAULT: 'Flama Medium'; font for the legend labels

- font_size:

  DEFAULT: 12; font size for legend labels

- label_color:

  DEFAULT: 'white'; text color for the legend labels

- position:

  DEFAULT: 'center'; rough position on the slide where the legend will
  be placed. Must be either 'center', 'left', or 'right'.

## Examples

``` r
# Before adding a legend onto a powerpoint,
# you must first read a PowerPoint into R
if (FALSE) { # \dontrun{
doc <- read_pptx('~/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Projects/
Qualtrics/2021 Template and Resources/Template for mscharts.pptx')

# Add a blank slide
doc <- add1s_y2()

# Add a legend
doc <- add_legend_y2(
  labels = c(
    'Agree', 
    'Neutral', 
    'Disagree'
  ),
  palette = c(
    'blue',
    'gray',
    'red'
  )
)

print(doc, '~/Desktop/test.pptx')
} # }
```
