# Create a grouped ggplot object

This function creates a ggplot2 object automatically formatted for a dot
plot with a grouping variable.

## Usage

``` r
gg_dotplot_y2(
  data = frequencies,
  x_var = result,
  y_var = label,
  color_var = group_var,
  alpha = 1,
  axis_text_size = 12,
  axis_title_size = 14,
  direction = c("horizontal", "vertical"),
  fills = "NULL",
  font_family = "Flama",
  label_length = 45,
  legend_pos = "top",
  legend_nrow = NULL,
  legend_rev = FALSE,
  legend_text_size = 8,
  legend_title_size = 8,
  legend_title = "",
  overwrite_breaks = TRUE,
  point_size = 6,
  title_label = "",
  title_size = 14,
  x_label = "",
  y_label = "",
  x_min = 0,
  x_max = 0
)
```

## Arguments

- data:

  DEFAULT = frequencies; The name of the data frame that ggplot pulls
  from.

- x_var:

  DEFAULT = result; When using the freqs function, will typically be
  result (is by default).

- y_var:

  DEFAULT = label; When using the freqs function, will typically be
  label (is by default).

- color_var:

  DEFAULT = group_var; Note: the color variable CANNOT be numeric.

- alpha:

  DEFAULT = 1; Range of 1-0 with 1 being completely opaque

- axis_text_size:

  DEFAULT = 12; Font size for variable levels and axis percentages.

- axis_title_size:

  DEFAULT = 14; Font size for x_label and y_label.

- direction:

  DEFAULT = 'horizontal'; Two options: "horizontal" (default) OR
  "vertical"

- fills:

  DEFAULT = 'NULL'; requires a vector of colors for all levels of the
  color_var/grouping variable. Otherwise, it will run fills as default
  ggplot2 colors.

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- label_length:

  DEFAULT = 45 for horizontal charts and 15 for vertical charts. This
  determines how many characters an x-axis label can be before R inserts
  a line break.

- legend_pos:

  DEFAULT = 'top'

- legend_nrow:

  DEFAULT = NULL; Change to a numeric to specify the number of rows for
  the legend

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

- point_size:

  DEFAULT = 6; Size for each point in the dot plot.

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart.

- title_size:

  DEFAULT = 18

- x_label, y_label:

  DEFAULT = ”; Title for the x_axis or y_axis

- x_min:

  DEFAULT = 0 to show full data without skewing perspective, but can be
  adjusted.

- x_max:

  DEFAULT = 0; however, the y_max automatically adjusts based on the max
  value of 'result', in most cases fitting the chart perfectly

## Examples

``` r
frequencies <- mpg %>%
  dplyr::group_by(manufacturer) %>%
  y2clerk::freqs(cty, hwy, stat = 'mean') %>%
  dplyr::mutate(label = variable) %>%
  orderlabel::order_label(
    group_var = group_var,
    inherent_order_group = TRUE
    )
#> Adding missing grouping variables: `manufacturer`
#> Adding missing grouping variables: `manufacturer`
chart <- gg_dotplot_y2(
  fills = c(
    "#F8766D", "#E58700", "#C99800", "#A3A500",
    "#6BB100", "#00BA38", "#00BF7D", "#00C0AF",
    "#00BCD8", "#00B0F6", "#619CFF", "#B983FF",
    "#E76BF3", "#FD61D1", "#FF67A4", "#F8766D"
  ),
  font_family = 'sans'
)
#> Error in gg_dotplot_y2(fills = c("#F8766D", "#E58700", "#C99800", "#A3A500",     "#6BB100", "#00BA38", "#00BF7D", "#00C0AF", "#00BCD8", "#00B0F6",     "#619CFF", "#B983FF", "#E76BF3", "#FD61D1", "#FF67A4", "#F8766D"),     font_family = "sans"): object 'frequencies' not found
chart
#> Error: object 'chart' not found
```
