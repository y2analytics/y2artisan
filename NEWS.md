# y2artisan 0.8.1
## New features
* Functions used to automatically create and populate powerpoint slides with charts (`add_section_header_y2`, `add1s_y2`, and `add1c_y2` can now be used to automate municipal style reports of a given style guide. Functions have a new argument called "report_style" which can be used to specify the style of report being made.
## New Functions
* `add_legend_y2` - Creates a powerpoint table legend with colored legend keys using flextable and adds it to the report


# y2artisan 0.8.0
## Breaking changes
* all gg_* functions now have the default font of "Flama" rather than "flama"
## New features
* `gg_donut_y2` has a new argument *center_label* with a default of 'NULL'. When set to 'NULL', each individual level of the donut chart will have a label over its respective section. If set to a specific value of the color_var, that percentage will appear in the center without a label, and no other labels will appear on the donut chart.
## Bug fixes
* Deprecated ggplot code fixed for gg_* functions. Better warnings for missing fonts


# y2artisan 0.7.0
## New Functions
* `gg_donut_y2` - Creates a donut chart ggplot object
## Breaking changes
* `footer_y2` has slightly new behavior for the *label_length*, which now truncates after 100 characters instead of 15, and it works on all question types, not just matrix-style questions. Additionally, the "n" that is counted for each question now excludes "NA" answers from open-ended questions and does not count them as responses that have not answered the question.
* Dropping `ms_footer_y2` now that we've had its newer version `footer_y2` for several versions. 
## Bug fixes 
* footer_y2: Updates to documentation. Fixes to removal of white space and hard returns. Updated coding practices to reflect new version of tidyselect. Lots of tests added that led to finding these bugs and making these updates.
* add_* functions: updated documentation 
* openend_y2: filled in required arguments in internal separate function



# y2artisan 0.6.4
## New Functions
*`add_colors_facebook_y2` - Add the standard Facebook color palette to your environment.
## New Features
* `ms_line_y2` - New argument _smooth_: DEFAULT = 0. If 0, the lines will be straight between points. If 1, the lines will be smoothed.
* `add_fonts_y2` - New argument *show_fonts_added*: DEFAULT = FALSE. If TRUE, shows the available fonts loaded in by add_fonts_y2().


# y2artisan 0.6.3
## Function Updates
* `add1s_y2` - corrected footer to allow it to be editable without zooming in on the powerpoint.
## New Functions
* `add_fonts_y2` - Use add_fonts_y2() to add the most commonly used fonts at Y2 to your Rsession. This will make them easily available for charting in ggplot and other instances where fonts may be needed. Use list_fonts_y2() to see which fonts will be loaded to your session.
* `list_fonts_y2` - Use list_fonts_y2() to see the available fonts loaded in by add_fonts_y2().


# y2artisan 0.6.2
## Function Updates
* `set_color_settings` - added argument "single" to allow function use for single bar charts. While not needed, this add matches syntax of `set_text_settings()`.
## Bug Fixes
* `footer_y2` - Fixed bug that treated single selects with a numeric ending as part of a matrix even if it wasn't.


# y2artisan 0.6.1
## Function Updates
* `ms_scatter_y2` - automatically sets axis titles to variable names unless otherwise specified
## Bug Fixes
* `footer_y2` - Fixed various notes that may cause edge-case errors
* `gg_dotplot_y2` - Fixed bug where legend and axis titles not updating properly


# y2artisan 0.6.0
## New functions
* `footer_y2` - Total function overhaul including breaking changes. Several added arguments and functionalities.


# y2artisan 0.5.9
## New functions
* `footer_y2` - Wrapper around ms_footer. Pipe in the dataset (responses), specify the variable, and set the question type. Save out the nicely formatted string to an object to pass to the footer of an ms chart
* `ms_print` - Easily preview ms_chart objects. By default prints "chart" in environment
## New arguments
* `set_color_settings_y2` - added option to select different column for colors. Increases ease of use for grouped type charts

# y2artisan 0.5.9
## New functions
* ms_scatter_y2 - Creates an mschart object automatically formatted for a scatter plot. This function requires two lists called "text_settings" and "color_settings" and a named vector called "size_settings".
## New arguments
* gg_dotplot_y2 - added alpha argument.


# y2artisan 0.5.8
## New functions
* gg_dotplot_y2 - Creates a ggplot2 object automatically formatted for a dot plot with a grouping variable.
## New arguments
* theme_y2 - added font_family argument.


# y2artisan 0.5.7
## Bug fixes
* All ms_* chart functions had a bug where they would throw an error if any NAs were present in the data. They will now run fine regardless if it has NAs.


# y2artisan 0.5.6
## New arguments
* `set_text_settings_y2` New arguments added. "text_column" allows you to select which column in the dataframe you wish to use to make your text settings. When you "single" to TRUE it will make text settings usable for a single bar chart


# y2artisan 0.5.5
## New functions and bug fixes
* `set_text_settings_y2` No longer do you need suffer the needless of pains of specifying every label text setting only to have your heart despair at another error (hint: it's probably a typo). This function not only reduces your typed amount, it reduces your human error. Save a frequencies object using freqs(). The text settings will be automatically matched to the "label" column of the frequencies.
* `set_color_settings_y2` Serving alongside it brother in arms, set_color_settings will free you from the chains of endless color naming vectors of death. Save a frequencies object using freqs(). Provide colors to the set_color_settings function. The colors will be automatically matched to the "label" column of the frequencies.
* `ms_footer_y2` Tired of copying and pasting question wording and n-size to your slides? This is the function for you! Pipe in the dataset (responses), specify the variable, and set the question type. Save out the nicely formatted string to an object to pass to the footer of an ms chart.


# y2artisan 0.5.4
## New function
* `theme_y2` Add the classic y2 theme to any ggplot2 chart: All y2artisan charts already have the y2 theme built in, but if you're creating a more custom ggplot2 chart and want to add the classic y2 theme, use theme_y2()


# y2artisan 0.5.3
## New function arguments
* `add1s_y2`: 1) title_color, commentary_color, and footer_color - can now specify font colors, 2) font_family - can now specify font for title, commentary, and footer on slide
* `add1c_y2`: same updates as add1s_y2


# y2artisan 0.5.2
## New function
* `add_colors_microsoft_y2`: Add the standard Microsoft color palette to your environment.


# y2artisan 0.5.0
## Breaking changes
* All defaults for `add*_y2` functions updated to match new Qualtrics template, as of March 2021 (new slide name defaults and positioning for new template)
* Default fonts for all `ms*_y2` functions updated to "BentonSans Regular" from Arial,  to match new Qualtrics template
* `add1t_y2` and `add2t_y2` functions deleted. Instead, use `add1c_y2` and `add2c_y2`  because they now also add tables to PowerPoint, not just charts



# y2artisan 0.4.4
## New function
* `add_colors_internal_y2`: Add the Y2 color palettes to your environment.

## Update
* Updated `add_colors_qualtrics_y2` to reflect new company template. Old colors still available. Also added argument "show_colors" to allow seeing colors loaded or not. 



# y2artisan 0.4.3
## New arguments
* `add_color_palette_y2`: Position argument added allowing for choosing where in the 5 point scale the chosen color is located. 

## Improvement
* Updated error and warning messages to control for unintended errors.



# y2artisan 0.4.2
## New functions
* `add_color_palette_y2`: Give a list of color labels (e.g. blue) and the associated hex codes (e.g. #41536F). For each color given, add_color_palette_y2 will add a 5 color, monochromatic scale.

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
