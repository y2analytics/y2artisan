# Add base Qualtrics colors

4 Qualtrics colors: PURPLE, GREEN, BLUE, and TEAL

## Usage

``` r
add_colors_qualtrics_y2(
  palette = 2,
  case = c("upper", "lower"),
  show_colors = TRUE
)
```

## Arguments

- palette:

  DEFAULT = 2. Defaults to the newest Qualtrics color palette (2).
  Switch between the latest color palette and older color palettes, with
  1 being the oldest.

- case:

  DEFAULT = 'upper'. Arg available only for palette 1. Will set color
  vectors to uppercase (e.g. BLUE_DARK) Can switch to 'lower' to revert
  to lowercase (e.g. blue_dark; older package versions were lowercase)

- show_colors:

  DEFAULT = TRUE Show the color palettes loaded into your environment

## Details

Adds to your environment pre-set hex codes for the 4 base colors
typically used in Qualtrics projects. Each of the 4 colors has 5 shades.
Also adds grays, black, and white.

## Examples

``` r
add_colors_qualtrics_y2()

add_colors_qualtrics_y2('lower')
```
