# Print an ms_chart object for your viewing pleasure

Provide a chart object created using the y2artisan suite of ms_charts
function to see a preview of the chart.

## Usage

``` r
ms_print_y2(chart_name = chart)
```

## Arguments

- chart_name:

  DEFAULT = chart; A chart object made using y2artisan or ms_charts

## Examples

``` r
frequencies <- mtcars %>%
  y2clerk::freqs(carb) %>%
  orderlabel::order_label(inherent_order_label = TRUE)

color_settings <- list('blue')
text_settings<- list('result' = officer::fp_text(font.size = 20))

chart <- ms_single_y2()
#> Error in ms_single_y2(): object 'frequencies' not found
ms_print_y2()
#> NULL
```
