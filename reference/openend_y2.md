# Look at the frequencies of each word in an open end question

Breaks down an open ended question on spaces, giving you the frequencies
of each word mentioned

## Usage

``` r
openend_y2(dataset, variable, top_x = 50)
```

## Arguments

- dataset:

  no default. Usually piped in from your main dataset

- variable:

  The name of the openended variable from your dataset you want to look
  at

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

responses %>% openend_y2(var1)
#> # A frequency tibble: 18 × 6
#>    variable value     label         n stat    result
#>    <chr>    <chr>     <chr>     <int> <chr>    <dbl>
#>  1 Words    DOGS      DOGS          6 percent   1   
#>  2 Words    CATS      CATS          3 percent   0.5 
#>  3 Words    COLLARS   COLLARS       2 percent   0.33
#>  4 Words    TOO       TOO           2 percent   0.33
#>  5 Words    AIGHT     AIGHT         1 percent   0.17
#>  6 Words    ALWAYS    ALWAYS        1 percent   0.17
#>  7 Words    COOL      COOL          1 percent   0.17
#>  8 Words    COOLER    COOLER        1 percent   0.17
#>  9 Words    LAST      LAST          1 percent   0.17
#> 10 Words    LIKE      LIKE          1 percent   0.17
#> 11 Words    ONE       ONE           1 percent   0.17
#> 12 Words    OVER      OVER          1 percent   0.17
#> 13 Words    PREFER    PREFER        1 percent   0.17
#> 14 Words    SENTENCE  SENTENCE      1 percent   0.17
#> 15 Words    TALK      TALK          1 percent   0.17
#> 16 Words    THAN      THAN          1 percent   0.17
#> 17 Words    TIGHT     TIGHT         1 percent   0.17
#> 18 Words    TYPICALLY TYPICALLY     1 percent   0.17
```
