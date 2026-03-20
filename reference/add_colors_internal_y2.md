# Add base Y2 colors

3 Y2 colors: BLUE, GRAY and ORANGE

## Usage

``` r
add_colors_internal_y2(palette = 1, compliments = FALSE, show_colors = TRUE)
```

## Arguments

- palette:

  DEFAULT = 1. Defaults to the newest Y2 color palette (1). Switch
  between the latest color palette and older color palettes, with 1
  being the oldest. Currently only 1 available.

- compliments:

  DEFAULT = FALSE. Add complimentary colors chosen by the Y2 team (Red,
  Pink, Yellow, Green, Turquoise)

- show_colors:

  DEFAULT = TRUE Show the color palettes loaded into your environment

## Details

Adds to your environment pre-set hex codes for the 3 base colors used in
Y2 themed projects. Each of the 3 colors has 5 shades. Also adds black
and white.

## Examples

``` r
add_colors_internal_y2()

add_colors_internal_y2(TRUE)
```
