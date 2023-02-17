#### openend_y2 (wc_prepper) ####
### Description
#' Look at the frequencies of each word in an open end question
#'
#' Breaks down an open ended question on spaces, giving you the frequencies of each word mentioned
#' @param dataset no default. Usually piped in from your main dataset
#' @param variable The name of the openended variable from your dataset you want to look at
#' @param top_x DEFAULT = 50; Shows the top X most commonly mentioned words you want to see from the open-end
#' @keywords openend open end frequencies freqs
#' @export
#' @examples
#' responses <- tibble::tibble(
#'   var1 = c(
#'     'I like to talk about dogs',
#'     'Dogs are cool but cats are aight too',
#'     'I prefer dogs over cats',
#'     "My dog's collars are always too tight",
#'     'One last sentence about dogs',
#'     'Cats collars are typically cooler than dogs'
#'   )
#' )
#'
#' responses %>% openend_y2(var1)

openend_y2 <- function(
  dataset,
  variable,
  top_x = 50
) {
  Names <- Words <- NULL
  var_flag <- dplyr::enquo(variable)
  frequencies <- dataset %>%
    dplyr::select(
      !!var_flag
    ) %>%
    dplyr::mutate(
      variable = toupper(!!var_flag),
      variable = stringr::str_split(variable, " ") # split
      ) %>%
    dplyr::mutate(variable = purrr::map(.$variable, ~unique(.x))) %>% # drop duplicates
    dplyr::mutate(variable = purrr::map_chr(.$variable, ~paste(.x, collapse = " "))) %>% # recombine
    dplyr::select(
      'variable'
    ) %>%
    tidyr::separate(
      .data$variable,
      into = paste("V", 1:100, sep = "_"),
      sep = ' '
    ) %>%
    tidyr::gather(
      Names,
      Words
    ) %>%
    dplyr::filter(
      Words != ''
    ) %>%
    dplyr::select(
      -tidyr::all_of(Names)
    ) %>%
    dplyr::mutate(
      Words = stringr::str_replace_all(Words, '\\.COM', ''),
      Words = stringr::str_replace_all(Words, "[^[:alnum:]]", ""),
      Words = stringr::str_replace_all(Words, 'DO NOT', 'DONT'),
      Words = stringr::str_replace_all(Words, 'CAN NOT', 'CANT'),
      Words = stringr::str_replace_all(Words, 'CANNOT', 'CANT')
    ) %>%
    y2clerk::freqs(
      Words
    ) %>%
    wc_filter() %>%
    dplyr::filter(
      .data$label != ''
    ) %>%
    dplyr::arrange(
      dplyr::desc(.data$n)
    ) %>%
    dplyr::slice(
      1:top_x
    ) %>%
    dplyr::mutate(
      total =  dataset %>%
        dplyr::filter(
          !is.na(!!var_flag),
          !!var_flag != ''
        ) %>%
        dplyr::count() %>%
        as.numeric(),
      result = .data$n / .data$total,
      result = round(.data$result, 2)
    ) %>%
    dplyr::select(
      -'total'
    )

  return(frequencies)
}

#### wc_filter ####
wc_filter <- function(dataset) {
  dataset %>%
    dplyr::filter(
      !.data$label %in% c(
        'AND',
        'THE',
        'I',
        'THAT',
        'TO',
        'A',
        'IT',
        'OF',
        'IS',
        'ARE',
        'ITS',
        'IN',
        'BE',
        'AS',
        'SO',
        'AN',
        'IF',
        'BY',
        'AM',
        'AT',
        'IM',
        'NA',
        'DO',
        'THIS',
        'OR',
        'FOR',
        'YES',
        'NO',
        'NOT',
        'NONE',
        'REALLY',
        'NOTHING',
        'ALL',
        'ME',
        'HE',
        'SHE',
        'MY',
        'WHEN',
        'CAN',
        'ON',
        'DONT',
        'KNOW',
        'IDK',
        'VERY',
        'WITH',
        'FROM',
        'WHO',
        'BUT',
        'WHOM',
        'ALSO',
        'OUR',
        'WOULD',
        'HAVE',
        'WE',
        'BACK',
        'HAS',
        'HAD',
        'YOU',
        'THERE',
        'WAS',
        'HERE',
        'SHOULD',
        'JUST',
        'THEIR',
        'THEM',
        'ABOUT',
        'THEY',
        'WILL',
        'BECAUSE'
      )
    )
}
