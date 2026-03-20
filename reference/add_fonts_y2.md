# Adds fonts and font families to your R session

Use add_fonts_y2() to add the most commonly used fonts at Y2 to your
Rsession. This will make them easily available for charting in ggplot
and other instances where fonts may be needed. Use list_fonts_y2() to
see which fonts will be loaded to your session.

## Usage

``` r
add_fonts_y2(show_fonts_added = FALSE)
```

## Arguments

- show_fonts_added:

  DEFAULT = FALSE. If TRUE, shows the available fonts loaded in by
  add_fonts_y2().

## Value

A message

## Examples

``` r
add_fonts_y2()
#> [1] "A font file in font family BentonSans not found. Download and install if you need this font."
#> [1] "A font file in font family Flama not found. Download and install if you need this font."
#> [1] "A font file in font family Roboto not found. Download and install if you need this font."
#> [1] "A font file in font family Pokemon not found. Download and install if you need this font."
#> [1] "A font file in font family Arial not found. Download and install if you need this font."
#> [1] "A font file in font family FreightSans not found. Download and install if you need this font."
#> [1] "A font file in font family Neutraface not found. Download and install if you need this font."
#> [1] "A font file in font family Proxima Nova not found. Download and install if you need this font."
#> [1] "A font file in font family Trade Gothic not found. Download and install if you need this font."
#> [1] "A font file in font family Century Gothic not found. Download and install if you need this font."
#> [1] "A font file in font family Circular not found. Download and install if you need this font."
#> [1] "A font file in font family Facebook Reader not found. Download and install if you need this font."
#> [1] "A font file in font family Optimist not found. Download and install if you need this font."
#> [1] "A font file in font family Proxima Nova not found. Download and install if you need this font."
#> [1] "A font file in font family Segoe UI not found. Download and install if you need this font."
#> [1] "A font file in font family Montserrat not found. Download and install if you need this font."
#> [1] "A font file in font family Gill Sans not found. Download and install if you need this font."
#> [1] "All available fonts loaded into Rsession"
```
