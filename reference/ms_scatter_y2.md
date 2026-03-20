# Create a scatter plot mschart object

This function creates an mschart object automatically formatted for a
scatter plot. It requires two lists called "text_settings" and
"color_settings" and a named vector called "size_settings".

## Usage

``` r
ms_scatter_y2(
  data = frequencies,
  x_var,
  y_var,
  group_var = NULL,
  label_var = "label",
  axis_text_size = 10,
  axis_title_size = 18,
  axis_x_display = TRUE,
  axis_x_label = "x_var",
  axis_x_min = 0,
  axis_x_max = NULL,
  axis_x_rotate = 0,
  axis_x_rotate_title = 0,
  axis_y_display = TRUE,
  axis_y_label = "y_var",
  axis_y_min = 0,
  axis_y_max = NULL,
  axis_y_rotate = 0,
  axis_y_rotate_title = 0,
  font_family = "BentonSans Regular",
  label_color = color_settings,
  label_position = "t",
  label_show_values = FALSE,
  label_text = text_settings,
  legend_pos = c("b", "n", "t", "tr", "l", "r"),
  legend_text_size = 12,
  num_fmt = c("percent", "general"),
  point_size = size_settings,
  title_label = "",
  title_size = 18
)
```

## Arguments

- data:

  DEFAULT = frequencies;The name of the data frame that the mschart
  pulls from.

- x_var:

  NO DEFAULT

- y_var:

  NO DEFAULT

- group_var:

  DEFAULT = NULL; Can be set to a variable to color code points by that
  variable.

- label_var:

  DEFAULT = 'label'; This variable will be the text labels that show up
  on the chart over each point.

- axis_text_size:

  DEFAULT = 10; Font size for variable levels and percentages.

- axis_title_size:

  DEFAULT = 18; Font size for axis_x_label and axis_y_label.

- axis_x_display, axis_y_display:

  DEFAULT = TRUE

- axis_x_label, axis_y_label:

  DEFAULT is automatically set to variable names for x_var and y_var;
  Title for the x_axis and y_axis

- axis_x_min, axis_y_min:

  DEFAULT = 0 to show full data without skewing perspective, but can be
  adjusted.

- axis_x_max, axis_y_max:

  DEFAULT = NULL

- axis_x_rotate, axis_y_rotate:

  DEFAULT = 0; Rotation of axis text. Set to -45 for diagonal, giving
  more space for text.

- axis_x_rotate_title, axis_y_rotate_title:

  DEFAULT = 0, set y_axis rotation to 360 for horizontal text

- font_family:

  DEFAULT = 'BentonSans Regular' (Qualtrics font). Sets the fonts for
  axis, legends, and titles. Label font is set within label_color and
  label_text lists. May specify fonts in quotes, e.g. "Times New Roman"

- label_color:

  DEFAULT = color_settings; A list of color settings for the points Can
  apply to all points or those of a specific group

- label_position:

  DEFAULT = 't'; Specifies the position of the data label. It should be
  one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When
  grouping is 'clustered', it should be one of
  'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should
  be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it
  should be one of 'b','ctr','l','r','t'.

- label_show_values:

  DEFAULT = FALSE; TRUE or FALSE. Show percent labels alongside each
  point label.

- label_text:

  DEFAULT = text_settings; A list of text settings for the percent
  labels. This affects font size and color. Specified outside of the
  function. If a list of one, no need to specify values. Otherwise, they
  must exactly match the group_var levels. Example: text_settings \<-
  list(fp_text(font.size = 10.5, color = bluepurple))

- legend_pos:

  DEFAULT = 'b' for bottom; All legend positions are 'b', 'n', 't',
  'tr', 'l', 'r'.

- legend_text_size:

  DEFAULT = 12

- num_fmt:

  DEFAULT = 'percent'; Can also be set to 'general' for non-percentages.
  Changes formatting for both the labels and axis

- point_size:

  DEFAULT = size_settings; A named vector of size settings. Must be
  between 2-72, typically around 10. Can be a vector of one unnamed
  number if all points are the same size.

- title_label:

  DEFAULT = ”; Add your title in "" as the title of the chart.

- title_size:

  DEFAULT = 18

## Examples

``` r
### Grouped
color_settings <- list(
  '0' = '#082C51',
  '1' = '#3E85BD'
)
size_settings <- c(
  '0' = 10,
  '1' = 5
)
text_settings <- list(
  '0' = officer::fp_text(color =  '#082C51'  , font.family = 'BentonSans Regular', font.size = 10),
  '1' = officer::fp_text(color =  '#3E85BD' , font.family = 'BentonSans Regular', font.size = 10)
)
frequencies <- mtcars %>%
  dplyr::select(mpg, wt, am) %>%
  dplyr::mutate(
    mpg = mpg/100,
    wt = wt/10,
    label = rownames(.)
  )
chart_scatter <- ms_scatter_y2(
  x_var = 'mpg',
  y_var = 'wt',
  group_var = 'am'
)
#> Error in ms_scatter_y2(x_var = "mpg", y_var = "wt", group_var = "am"): object 'frequencies' not found
print(chart_scatter, preview = TRUE)
#> Error: object 'chart_scatter' not found

# Ungrouped
color_settings <- list('#082C51')
size_settings <- c(10)
text_settings <- list('wt' = officer::fp_text(font.family = 'BentonSans Regular'))
frequencies <- mtcars %>%
  dplyr::select(mpg, wt) %>%
  dplyr::mutate(
    mpg = mpg/100,
    wt = wt/10,
    label = rownames(.)
  )
chart_scatter <- ms_scatter_y2(
  x_var = 'mpg',
  y_var = 'wt'
)
#> Error in ms_scatter_y2(x_var = "mpg", y_var = "wt"): object 'frequencies' not found
print(chart_scatter, preview = TRUE)
#> Error: object 'chart_scatter' not found
```
