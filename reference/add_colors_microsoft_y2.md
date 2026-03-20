# Add base Microsoft colors

Microsoft colors: ORANGE, YELLOW, GREEN, TEAL, BLUE, PURPLE, GRAY

## Usage

``` r
add_colors_microsoft_y2(palette = c("RE&S")[1], show_colors = TRUE)
```

## Arguments

- palette:

  DEFAULT = 1. Defaults to the newest microsoft color palette (1).
  Switch between the latest color palette and older color palettes, with
  1 being the oldest. Currently only 1 available.

- show_colors:

  DEFAULT = TRUE Show the color palettes loaded into your environment

## Details

Adds to your environment pre-set hex codes for the 7 base colors used in
Microsoft themed projects. Each of the 7 colors has 3 shades except
Gray, which has 6 shades. Also adds black and white.

## Examples

``` r
add_colors_microsoft_y2()

add_colors_microsoft_y2(show_colors = TRUE)
```
