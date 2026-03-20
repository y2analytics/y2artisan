# Create a pie chart ggplot object

This function creates a ggplot2 object automatically formatted for a pie
chart.

## Usage

``` r
gg_pie_y2(
  data = frequencies,
  x_var = label,
  y_var = result,
  colors = "#ffffff",
  fills,
  font_family = "Flama",
  label_length = 15,
  label_size = 10,
  legend_pos = "none",
  legend_rev = FALSE,
  legend_text_size = 8,
  overwrite_breaks = TRUE,
  title_label = "",
  title_size = 14
)
```

## Arguments

- data:

  DEFAULT = frequencies; The name of the data frame that ggplot pulls
  from.

- x_var:

  DEFAULT = label; When using the freqs function, will typically be
  label (is by default).

- y_var:

  DEFAULT = result; When using the freqs function, will typically be
  result (is by default).

- colors:

  DEFAULT is white ('#ffffff') for the text of all percent labels; You
  may also 1) Specify 1 color, and this color will be applied to all
  color_var levels or 2) Specify a vector of colors for each individual
  level of the color_var

- fills:

  NO DEFAULT; requires a vector of colors for all levels of the x_var

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- label_length:

  DEFAULT = 15; This determines how many characters a label on a pie
  slice can be before R inserts a line break.

- label_size:

  DEFAULT = 10. Adjusts the size of the percent labels over each bar.

- legend_pos:

  DEFAULT = 'none'

- legend_rev:

  DEFAULT = FALSE

- legend_text_size:

  DEFAULT = 8

- overwrite_breaks:

  DEFAULT = TRUE, Whether to overwrite existing linebreaks in string
  label inputs when performing text pre-processing, such as string
  wrapping and whitespace removal

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart.

- title_size:

  DEFAULT = 18

## Examples

``` r
frequencies <- iris %>% y2clerk::freqs(Species)

chart <- gg_pie_y2(
  fills = c('red', 'blue', 'pink'),
  font_family = 'sans'
)
#> Error in gg_pie_y2(fills = c("red", "blue", "pink"), font_family = "sans"): object 'frequencies' not found
```
