# Create an ugrouped mschart object

This function creates a mschart object automatically formatted for a
single variable (including multiple select). It requires two lists
called "text_settings" and "color_settings" by default that specify the
colors desired for the chart.

## Usage

``` r
ms_single_y2(
  data = frequencies,
  x_var = "label",
  y_var = "result",
  axis_text_size = 10,
  axis_title_size = 18,
  axis_x_display = TRUE,
  axis_x_label = "",
  axis_x_position = c("nextTo", "high", "low", "none"),
  axis_x_rotate = 0,
  axis_x_rotate_title = 0,
  axis_y_display = FALSE,
  axis_y_label = "",
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_rotate = 0,
  axis_y_rotate_title = 0,
  direction = c("vertical", "horizontal"),
  font_family = "BentonSans Regular",
  gap_width = 25,
  grouping = "standard",
  group_var = NULL,
  label_color = color_settings,
  label_position = c("outEnd", "inEnd", "ctr", "inBase"),
  label_show_values = TRUE,
  label_text = text_settings,
  legend_pos = c("n", "t", "b", "tr", "l", "r"),
  legend_text_size = 16,
  num_fmt = c("percent", "general"),
  overlapping = -50,
  title_label = "",
  title_size = 18
)
```

## Arguments

- data:

  DEFAULT = frequencies;The name of the data frame that the mscharts
  pulls from.

- x_var:

  DEFAULT = 'label'; When using the freqs function, will typically be
  label (is by default).

- y_var:

  DEFAULT = 'result'; When using the freqs function, will typically be
  result (is by default).

- axis_text_size:

  DEFAULT = 10; Font size for variable levels and percentages.

- axis_title_size:

  DEFAULT = 18; Font size for axis_x_label and axis_y_label.

- axis_x_display, axis_y_display:

  DEFAULT = TRUE for x axis, DEFAULT = FALSE for y axis

- axis_x_label, axis_y_label:

  DEFAULT = ”; Title for the x_axis and y_axis

- axis_x_position:

  DEFAULT = 'nextTo'; Other options include "high", "low", "none".
  Change to "low" if dealing with negative numbers

- axis_x_rotate, axis_y_rotate:

  DEFAULT = 0; Rotation of axis text. Set to -45 for diagonal, giving
  more space for text.

- axis_x_rotate_title, axis_y_rotate_title:

  DEFAULT = 0, set y_axis rotation to 360 for horizontal text

- axis_y_min:

  DEFAULT = 0 to show full data without skewing perspective, but can be
  adjusted.

- axis_y_max:

  DEFAULT = NULL

- direction:

  DEFAULT = 'vertical'; Two options: "vertical" (default) OR
  "horizontal"

- font_family:

  DEFAULT = 'BentonSans Regular' (Qualtrics font). Sets the fonts for
  axis, legends, and titles. Label font is set within label_color and
  label_text lists. May specify fonts in quotes, e.g. "Times New Roman"

- gap_width:

  DEFAULT = 25, meaning the size of the space between bars is 25% the
  size of the bar itself

- grouping:

  DEFAULT = 'standard'; grouping for a barchart, a linechart or an area
  chart. must be one of "percentStacked", "clustered", "standard" or
  "stacked".

- group_var:

  DEFAULT = NULL; If you want the bars to be different colors, set
  group_var to the same variable as x_var. Then set overlap to 100.

- label_color:

  DEFAULT = color_settings; A list of color settings for the bars. This
  affects font size and color. Specified outside of the function. If a
  list of one, no need to specify values. Otherwise, they must exactly
  match the group_var levels. Example: color_settings \<-
  list(bluepurple)

- label_position:

  DEFAULT = 'outEnd'; Other options include c('outEnd', 'inEnd', 'ctr',
  'inBase')

- label_show_values:

  DEFAULT = TRUE; TRUE or FALSE. Show percent labels for each value.

- label_text:

  DEFAULT = text_settings; A list of text settings for the percent
  labels. This affects font size and color. Specified outside of the
  function. If a list of one, no need to specify values. Otherwise, they
  must exactly match the group_var levels. Example: text_settings \<-
  list("result" = fp_text(font.size = 10.5, color = bluepurple,
  font.family = 'Arial'))

- legend_pos:

  DEFAULT = 'n' for none. Other legend positions are 'b', 'tr', 'l',
  'r', 't'.

- legend_text_size:

  DEFAULT = 16

- num_fmt:

  DEFAULT = 'percent'; Can also be set to 'general' for non-percentages.
  Changes formatting for both the labels and axis

- overlapping:

  DEFAULT = -50; This leaves 50% extra space between variable levels.
  Set to 100 when coloring bars different colors.

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart.

- title_size:

  DEFAULT = 18

## Examples

``` r
frequencies <- mtcars %>%
  y2clerk::freqs(carb) %>%
  orderlabel::order_label(inherent_order_label = TRUE)

color_settings <- list('blue')
text_settings<- list('result' = officer::fp_text(font.size = 20))

chart <- ms_single_y2()
#> Error in ms_single_y2(): object 'frequencies' not found
print(chart, preview = TRUE)
#> Error: object 'chart' not found
```
