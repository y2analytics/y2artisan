# y2artisan pre 0.3.0
## New functions
* `gg_histo_y2`
* `gg_pie_y2`
* `gg_maxdiff_y2`
* `add_colors_y2`

## Argument changes
* combined axis_num_fmt and label_num_fmt into one argument: `num_fmt`
* `font_family` - new argument
* `legend_row` - new argument
* `axis_position` - new argument
* `chart_width` and `chart_height`
* `chart_path` to ggchart_save_y2

## New files
* NEWS.md
* README.md
* cran-comments.md

## Bugs
* `wordcloud_y2` no longer breaking
* `axis_display` bugs fixed
* Better error messages, hopefully these help you solve your own problems ;) 
* More extensive documentation and examples. You're welcome



# y2artisan 0.1.1
## Breaking changes
New naming conventions:
1. All functions end with the suffix `_y2`
2. mschart based functions start with the prefix `ms_`



# y2artisan 0.1.0
## Overview
First release with the follow functions:

ms_chart functions:
1. add1c
2. add1s
3. add1t
4. add2c
5. add2t
6. add3c
7. add4c
8. add5c
9. add6c
10. add_section_header
11. bar_grouped
12. bar_single
13. bar_stacked
14. line_chart
15. max_dif

ggplot functions:
1. gg_grouped
2. gg_single
3. gg_stacked
4. ggchart_save
5. openend
6. wordcloud
7. wordcloud2
