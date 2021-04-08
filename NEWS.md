# y2artisan 0.4.2
## New functions
* `add_color_palette_y2`: Give a list of color labels (e.g. blue) and the associated hex codes (e.g. #41536F). For each color given, add_color_pallette_y2 will add a 5 color, monochromatic scale.

## Breaking changes
* Old function `add_colors_y2` name changed to `add_colors_qualtrics_y2`

## Improvements
* percent labels on gg_single_y2 and gg_grouped_y2 charts are now left justified instead of centered. Visually cleaner because %'s and single vs multiple digit #s were previously different distances from the bar



# y2artisan 0.4.1
## Bugs
* All examples now working
* Eliminated notes about missing global variables and functions



# y2artisan 0.4.0
## Breaking changes
* `add_colors_y2`
1. By default, color vectors are now in UPPER CASE (e.g. green is now GREEN)
2. Added set of GRAY colors
3. When working with old code, you can still use this function, just use the `case = 'lower'` argument to revert color vectors to lowercase 

* `ggchart_save_y2`
1. The function is now tidy, meaning the first argument is now chart (a ggplot2 chart object) instead of chartname (the file name you wish you save your chart as). Essentially the only change is that you should now pipe your chart object into the function first. 

## Bugs
* Fixed a bug in horizontal grouped charts where the chart legend was backwards from the order of the bars in the actual chart space
* Fixed internal package calls to avoid warnings/notes



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
