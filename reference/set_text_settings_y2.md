# Setup the text setting for ms charts

Save a frequencies object using freqs(). The text settings will be
automatically matched to the "label" column of the frequencies.

## Usage

``` r
set_text_settings_y2(
  dataset = frequencies,
  font_size = 14,
  font_color = "black",
  font_family = "BentonSans",
  text_column = label,
  single = FALSE
)
```

## Arguments

- dataset:

  DEFAULT = frequencies; A frequency table named frequencies

- font_size:

  DEFAULT = 14; Font size applied to all text in chart labels

- font_color:

  DEFAULT = 'black'; Color applied to all text in chart labels

- font_family:

  DEFAULT = 'BentonSans'; Font family applied to all text in chart
  labels

- text_column:

  DEFAULT = label; Column from which labels are pulled

- single:

  DEFAULT = FALSE; Set to TRUE for use with a single bar chart

## Examples

``` r
frequencies <-
  mtcars %>%
  y2clerk::freqs(
    cyl
  )

set_text_settings_y2()
#> Error in set_text_settings_y2(): object 'frequencies' not found

set_text_settings_y2(
  font_size = 28,
  font_color = 'black'
 )
#> Error in set_text_settings_y2(font_size = 28, font_color = "black"): object 'frequencies' not found
```
