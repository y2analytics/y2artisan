# Creates a word cloud from an open end question

Finds the frequencies of each word in an open end question and creates a
word cloud based on the frequencies. Words mentioned less are smaller
and lighter in color

## Usage

``` r
wordcloud_y2(
  dataset,
  variable,
  colors = "bluepurple",
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

  DEFAULT = 'bluepurple'; 4 qualtrics colors as pre-made options:
  "bluepurple", "lime", "teal", "brightblue". May also specify a vector
  of 3 scaled colors ranging from lightest to darkest

- max_size:

  DEFAULT = 12; the largest text size for the word with the highest
  frequency

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

RED_DARK <- '#C61616'
RED_MID <- '#E38B8B'
RED_LIGHT <- '#F9E6E6'

# If you want to use a specific color, you must specify three scalar shades of that color
responses %>% wordcloud_y2(var1, font_family = "Arial")
#> Error in wordcloud_y2(., var1, font_family = "Arial"): The font you specified in the 'font_family' argument does not exist in your R session
responses %>% wordcloud_y2(var1, c(RED_LIGHT, RED_MID, RED_DARK), font_family = "Arial",)
#> Error in wordcloud_y2(., var1, c(RED_LIGHT, RED_MID, RED_DARK), font_family = "Arial",     ): The font you specified in the 'font_family' argument does not exist in your R session
```
