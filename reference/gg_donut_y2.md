# Create a donut chart ggplot object

This function creates a ggplot2 object automatically formatted for a
donut chart

## Usage

``` r
gg_donut_y2(
  data = frequencies,
  color_var = label,
  y_var = result,
  center_label = "NULL",
  colors = "#ffffff",
  fills,
  font_family = "Flama",
  hole_size = 2.5,
  label_size = 5,
  legend_pos = "none",
  legend_rev = FALSE,
  legend_text_size = 8,
  title_label = "",
  title_size = 40
)
```

## Arguments

- data:

  DEFAULT = frequencies; The name of the data frame that ggplot pulls
  from

- color_var:

  DEFAULT = label; Note: the color variable CANNOT be numeric

- y_var:

  DEFAULT = result; When using the freqs function, will typically be
  result (is by default)

- center_label:

  DEFAULT = 'NULL'. When set to 'NULL', each individual level of the
  donut chart will have a label over its respective section. If set to a
  specific value of the color_var, that percentage will appear in the
  center, without a label, and no other labels will appear on the donut
  chart.

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

- hole_size:

  DEFAULT = 2.5. The larger the size, the larger the donut hole (minimum
  of 1; no maximum but higher than 10 is not recommended)

- label_size:

  DEFAULT = 10. Adjusts the size of the percent labels

- legend_pos:

  DEFAULT = 'none'

- legend_rev:

  DEFAULT = FALSE

- legend_text_size:

  DEFAULT = 8

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart

- title_size:

  DEFAULT = 40

## Examples

``` r
# Each level of the chart has a label
frequencies <- iris %>% y2clerk::freqs(Species)
chart <- gg_donut_y2(
  fills = c('red', 'blue', 'pink'),
  font_family = 'sans'
)
#> Error in gg_donut_y2(fills = c("red", "blue", "pink"), font_family = "sans"): object 'frequencies' not found

# Only one level of the chart has a percentage, in the center of the donut
frequencies <- ToothGrowth %>% y2clerk::freqs(supp)
chart <- gg_donut_y2(
  fills = c('orange', 'gray'),
  center_label = 'OJ',
  font_family = 'sans',
  title_label = 'OJ',
  label_size = 15,
)
#> Error in gg_donut_y2(fills = c("orange", "gray"), center_label = "OJ",     font_family = "sans", title_label = "OJ", label_size = 15,     ): object 'frequencies' not found
```
