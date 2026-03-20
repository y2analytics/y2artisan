# String Wrapping Function (Y2 Private)

Modified string wrapping helper function that does not over-write
linebreaks by default (see also the base R \`strwrap()\` function)

## Usage

``` r
strwrap_helper(
  x,
  width = width,
  overwrite_breaks = TRUE,
  indent = 0,
  exdent = 0,
  prefix = "",
  simplify = simplify,
  initial = prefix
)
```

## Arguments

- x:

  a character vector, or an object which can be converted to a character
  vector by \`as.character()\`.

- width:

  a positive integer giving the target column for wrapping lines in the
  output.

- overwrite_breaks:

  DEFAULT: TRUE; Whether the function overwrites existing linebreaks in
  an input string.

- indent:

  a non-negative integer giving the indentation of the first line in a
  paragraph.

- exdent:

  a non-negative integer specifying the indentation of subsequent lines
  in paragraphs.

- prefix, initial:

  a character string to be used as prefix for each line except the
  first, for which \`initial\` is used.

- simplify:

  a logical. If TRUE, the result is a single character vector of line
  text; otherwise, it is a list of the same length as x the elements of
  which are character vectors of line text obtained from the
  corresponding element of x. (Hence, the result in the former case is
  obtained by unlisting that of the latter.)
