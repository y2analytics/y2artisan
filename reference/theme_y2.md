# Add the classic y2 theme to any ggplot2 chart

All y2artisan charts already have the y2 theme built in, but if you're
creating a more custom ggplot2 chart and want to add the classic y2
theme, use theme_y2()

## Usage

``` r
theme_y2(font_family = "Flama")
```

## Arguments

- font_family:

  DEFAULT = 'Flama'

## Examples

``` r
if (FALSE) { # \dontrun{
frequencies <- mtcars %>%
  y2clerk::freqs(carb) %>%
  orderlabel::order_label(inherent_order_label = TRUE)

chart <- ggplot2::ggplot(
  frequencies,
  ggplot2::aes(x = label, y = result)
) +
  ggplot2::geom_bar(stat = 'identity') +
  theme_y2()
  } # }
```
