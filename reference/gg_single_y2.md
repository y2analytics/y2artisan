# Create an ungrouped ggplot object

This function creates a ggplot2 object automatically formatted for a
single variable (including multiple select).

## Usage

``` r
gg_single_y2(
  data = frequencies,
  x_var = label,
  y_var = result,
  label_var = percent_label,
  color_var = label,
  axis_text_size = 12,
  axis_title_size = 14,
  axis_y_display = TRUE,
  axis_x_display = TRUE,
  bar_width = 0.75,
  chart_height = 5.5,
  chart_width = 11,
  direction = c("vertical", "horizontal"),
  fills = "#474E7E",
  font_family = "Flama",
  label_length = 45,
  label_size = 10,
  legend_pos = "none",
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = "",
  nudge = 0,
  overwrite_breaks = TRUE,
  title_label = "",
  title_size = 14,
  x_label = "",
  y_label = "",
  y_min = 0,
  y_max = 0
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

- label_var:

  DEFAULT = percent_label; When using the order_label function, this
  variable will be created for you.

- color_var:

  DEFAULT = label; Although the color_var is set to label, the default
  for the "fills" argument sets all bars to be the same color if only a
  single color is specified. Note: the color variable CANNOT be numeric.

- axis_text_size:

  DEFAULT = 12; Font size for variable levels and axis percentages.

- axis_title_size:

  DEFAULT = 14; Font size for x_label and y_label.

- axis_x_display, axis_y_display:

  DEFAULT = TRUE; Set to FALSE to remove axis labels.

- bar_width:

  DEFAULT = .75, with a bar_width of 1 meaning each bars touches the
  ones next to it

- chart_height:

  DEFAULT = 5.5, If saving out a vertical bar chart with a different
  height, set the height here to have the nudge argument adjust itself
  automatically

- chart_width:

  DEFAULT = 11, If saving out a horizontal bar chart with a different
  width, set the width here to have the nudge argument adjust itself
  automatically

- direction:

  DEFAULT = 'vertical'; Two options: "vertical" (default) OR
  "horizontal"

- fills:

  DEFAULT = '#474E7E', the purple Qualtrics color. Three options: 1)
  Give only 1 color, all bars will be that color. 2) Specify the color
  for every bar using a vector of colors. 3) Set the "color_var"
  argument to a variable in the data frame and specify a color for each
  level in the color_var.

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- label_length:

  DEFAULT = 45 for horizontal charts and 15 for vertical charts. This
  determines how many characters an x-axis label can be before R inserts
  a line break.

- label_size:

  DEFAULT = 10. Adjusts the size of the percent labels over each bar.

- legend_pos:

  DEFAULT = 'none'

- legend_rev:

  DEFAULT = FALSE

- legend_text_size:

  DEFAULT = 8

- legend_title_size:

  DEFAULT = 8

- legend_title:

  DEFAULT = ”, If you put in a title, the legend will default to 'top'
  unless otherwise specified

- nudge:

  DEFAULT = 0; however, nudge automatically adjusts based on the max
  value of 'result', in most cases fitting the chart perfectly

- overwrite_breaks:

  DEFAULT = TRUE, Whether to overwrite existing linebreaks in string
  label inputs when performing text pre-processing, such as string
  wrapping and whitespace removal

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart.

- title_size:

  DEFAULT = 18

- x_label, y_label:

  DEFAULT = ”; Title for the x_axis or y_axis

- y_min:

  DEFAULT = 0 to show full data without skewing perspective, but can be
  adjusted.

- y_max:

  DEFAULT = 0; however, the y_max automatically adjusts based on the max
  value of 'result', in most cases fitting the chart perfectly

## Examples

``` r
frequencies <- mtcars %>%
  y2clerk::freqs(carb) %>%
  orderlabel::order_label(inherent_order_label = TRUE)

chart <- gg_single_y2(font_family = 'sans')
#> Error in gg_single_y2(font_family = "sans"): object 'frequencies' not found
```
