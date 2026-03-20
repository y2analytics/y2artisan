# Create a grouped ggplot object

This function creates a ggplot2 object automatically formatted for a
stacked variable with inherent ordering.

## Usage

``` r
gg_stacked_y2(
  data = frequencies,
  x_var = group_var,
  y_var = result,
  label_var = percent_label,
  color_var = label,
  axis_y_display = TRUE,
  axis_x_display = FALSE,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  colors = "0",
  direction = c("horizontal", "vertical"),
  erase_labels = 0.01,
  fills,
  font_family = "Flama",
  label_length = 45,
  label_size = 8,
  legend_nrow = NULL,
  legend_pos = "top",
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = "",
  overwrite_breaks = TRUE,
  title_label = "",
  title_size = 14,
  x_label = "",
  y_label = "",
  y_min = 0
)
```

## Arguments

- data:

  DEFAULT = frequencies; The name of the data frame that ggplot pulls
  from.

- x_var:

  DEFAULT = group_var; When doing a single stacked bar, set x_var to
  variable or any variable that has a only one level

- y_var:

  DEFAULT = result; When using the freqs function, will typically be
  result (is by default).

- label_var:

  DEFAULT = percent_label; When using the order_label function, this
  variable will be created for you.

- color_var:

  DEFAULT = label. Note: the color variable CANNOT be numeric.

- axis_y_display:

  DEFAULT = TRUE; for a single stacked bar, set to FALSE to remove axis
  labels

- axis_x_display:

  DEFAULT = FALSE

- axis_text_size:

  DEFAULT = 12; Font size for variable levels and axis percentages.

- axis_title_size:

  DEFAULT = 14; Font size for x_label and y_label.

- bar_width:

  DEFAULT = .75, with a bar_width of 1 meaning each bars touches the
  ones next to it

- colors:

  DEFAULT is white ('#ffffff') for the text of all percent labels; You
  may also 1) Specify 1 color, and this color will be applied to all
  color_var levels or 2) Specify a vector of colors for each individual
  level of the color_var

- direction:

  DEFAULT = 'horizontal'; Two options: "vertical" OR "horizontal"
  (default)

- erase_labels:

  DEFAULT = .01; all percent labels less than or equal to erase_labels
  will be erased to avoid clutter and overlapping labels. This argument
  pulls from the value in the result column of the dataframe being used

- fills:

  NO DEFAULT; requires a vector of colors for all levels of the
  color_var

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- label_length:

  DEFAULT = 45 for horizontal charts and 15 for vertical charts. This
  determines how many characters an x-axis label can be before R inserts
  a line break.

- label_size:

  DEFAULT = 8. Adjusts the size of the percent labels within each bar.

- legend_nrow:

  DEFAULT = NULL; Change to a numeric to specify the number of rows for
  the legend

- legend_pos:

  DEFAULT = 'top'

- legend_rev:

  DEFAULT = FALSE

- legend_text_size:

  DEFAULT = 8

- legend_title_size:

  DEFAULT = 8

- legend_title:

  DEFAULT = ”, If you put in a title, the legend will default to 'top'
  unless otherwise specified

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

  DEFAULT = 0. Change to a negative value such as -.2 to add space for
  extra text/graphic between the axis text and the stacked bars

## Examples

``` r
frequencies <- tibble::tibble(
  label = rep(c('Promoter', 'Passive', 'Detractor'), 3),
  result = c(.33, .33, .34, .20, .30, .50, .25, .50, .25),
  value = rep(c(1, 2, 3), 3),
  group_var = c(rep('Group A', 3), rep('Group B', 3), rep('Group C', 3))
) %>% orderlabel::order_label(
  group_var = group_var,
  stacked = 'gg'
)

chart <- gg_stacked_y2(
  fills = c('red', 'yellow', 'green'),
  font_family = 'sans'
)
#> Error in gg_stacked_y2(fills = c("red", "yellow", "green"), font_family = "sans"): object 'frequencies' not found


# For a single stacked bar:
frequencies <- tibble::tibble(
 label = c('Promoter', 'Passive', 'Detractor'),
 result = c(.33, .33, .34),
 value = c(1, 2, 3),
 variable = rep('QNPS', 3)
) %>% orderlabel::order_label(stacked = 'gg')

chart <- gg_stacked_y2(
   x_var = variable,
   axis_y_display = FALSE,
   fills = c('red', 'yellow', 'green'),
   font_family = 'sans'
)
#> Error in gg_stacked_y2(x_var = variable, axis_y_display = FALSE, fills = c("red",     "yellow", "green"), font_family = "sans"): object 'frequencies' not found
```
