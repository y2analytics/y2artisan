# Setup the color setting for ms charts

Save a frequencies object using freqs(). Provide colors to the
set_color_settings function. The colors will be automatically matched to
the "label" column of the frequencies or you can manually choose a
different column .

## Usage

``` r
set_color_settings_y2(
  ...,
  dataset = frequencies,
  color_column = label,
  single = FALSE
)
```

## Arguments

- ...:

  Colors to use in your ms_chart

- dataset:

  DEFAULT = frequencies; A frequency table named frequencies

- color_column:

  DEFAULT = label; Column from which labels are pulled

- single:

  DEFAULT = FALSE; Set to TRUE when making a single bar chart or when
  you need just one color.

## Examples

``` r
add_colors_internal_y2()


frequencies <-
  mtcars %>%
  y2clerk::freqs(
    cyl
  )

set_color_settings_y2(
  BLUE_DARKER,
  BLUE,
  BLUE_LIGHTER
 )
#> Error in set_color_settings_y2(BLUE_DARKER, BLUE, BLUE_LIGHTER): object 'frequencies' not found

frequencies <-
  mtcars %>%
  dplyr::group_by(
    gear
  ) %>%
  y2clerk::freqs(
    cyl
  )
#> Adding missing grouping variables: `gear`

set_color_settings_y2(
  BLUE_DARKER,
  BLUE,
  BLUE_LIGHTER,
  color_column = group_var
 )
#> Error in set_color_settings_y2(BLUE_DARKER, BLUE, BLUE_LIGHTER, color_column = group_var): object 'frequencies' not found
```
