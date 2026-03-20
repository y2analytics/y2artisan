# Creates a word cloud from an open end question

Finds the frequencies of each word in an open end question and creates a
word cloud based on the frequencies. Words mentioned less are smaller

## Usage

``` r
wordcloud_mono_y2(
  dataset,
  variable,
  colors = "#474E7E",
  max_size = 12,
  min_size = 1,
  font_family = "Flama",
  top_x = 50
)
```

## Arguments

- dataset:

  no default. Usually piped in from your main dataset

- variable:

  The name of the openended variable from your dataset you want to look
  at

- colors:

  DEFAULT = '#474E7E' (bluepurple from Qualtrics template). All words
  are the same color. Any color may be specified as a hexcode

- max_size:

  DEFAULT = 12; the largest text size for the word with the highest
  frequency#' @keywords openend open end wordcloud word cloud

- min_size:

  DEFAULT = 1; the smallest text size for the word with the lowest
  frequency

- font_family:

  DEFAULT = 'Flama'; all fonts used need to be previously loaded in
  using the font_add() and showtext_auto() functions

- top_x:

  DEFAULT = 50; Shows the top X most commonly mentioned words you want
  to see from the open-end

## Examples

``` r
responses <- tibble::tibble(
  var1 = c(
    'I like to talk about dogs',
    'Dogs are cool but cats are aight too',
    'I prefer dogs over cats',
    "My dog's collars are always too tight",
    'One last sentence about dogs',
    'Cats collars are typically cooler than dogs'
  )
)

responses %>% wordcloud_mono_y2(var1, font_family = "Arial")
#> Error in wordcloud_mono_y2(., var1, font_family = "Arial"): The font you specified in the 'font_family' argument does not exist in your R session
responses %>% wordcloud_mono_y2(var1, 'red', font_family = "Arial")
#> Error in wordcloud_mono_y2(., var1, "red", font_family = "Arial"): The font you specified in the 'font_family' argument does not exist in your R session
```
