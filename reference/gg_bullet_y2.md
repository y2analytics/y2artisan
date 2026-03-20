# Create a bullet chart: UNDER CONSTRUCTION

This function creates a ggplot2 object automatically formatted for an
overlapping bullet chart.

## Usage

``` r
gg_bullet_y2(
  data = frequencies,
  x_var = label,
  y_var = result,
  base_color,
  base_level,
  label_var = percent_label,
  color_var = group_var,
  axis_text_size = 12,
  axis_title_size = 14,
  bar_width = 0.75,
  chart_height = 5.5,
  chart_width = 11,
  direction = c("vertical", "horizontal"),
  fills,
  font_family = "Flama",
  label_length = 45,
  label_size = 6,
  legend_pos = "top",
  legend_nrow = NULL,
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = "",
  overwrite_breaks = TRUE,
  nudge = 0,
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

- base_color:

  NO DEFAULT; must specify a color for the base bar

- base_level:

  NO DEFAULT; specify in quotes the character string for the base lebel

- label_var:

  DEFAULT = percent_label; When using the order_label function, this
  variable will be created for you.

- color_var:

  DEFAULT = group_var. Note: the color variable CANNOT be numeric.

- axis_text_size:

  DEFAULT = 12; Font size for variable levels and axis percentages.

- axis_title_size:

  DEFAULT = 14; Font size for x_label and y_label.

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

  NO DEFAULT; requires a vector of colors for all levels of the
  color_var/grouping variable

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- label_length:

  DEFAULT = 45 for horizontal charts and 15 for vertical charts. This
  determines how many characters an x-axis label can be before R inserts
  a line break.

- label_size:

  DEFAULT = 6. Adjusts the size of the percent labels over each bar.

- legend_pos:

  DEFAULT = 'top'

- legend_nrow:

  DEFAULT = NULL; Change to a numeric to specify the number of rows for
  the legend

- legend_rev:

  DEFAULY = FALSE

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

- nudge:

  DEFAULT = 0; however, nudge automatically adjusts based on the max
  value of 'result', in most cases fitting the chart perfectly

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
frequencies <- ToothGrowth %>%
  dplyr::group_by(supp) %>%
  y2clerk::freqs(dose) %>%
  orderlabel::order_label(group_var = group_var)
#> Adding missing grouping variables: `supp`

chart <- gg_bullet_y2(
  base_level = 'VC',
  fills = c('orange', 'gray'),
  font_family = 'sans'
)
#> Error in gg_bullet_y2(base_level = "VC", fills = c("orange", "gray"),     font_family = "sans"): object 'frequencies' not found
```
