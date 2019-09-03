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
#' frequencies <- openend_y2(QOPEN_END)

openend_y2 <- function(
  dataset,
  variable,
  top_x = 50
) {
  flag <- dplyr::enquo(variable)
  frequencies <- dataset %>%
    select(
      !!flag
    ) %>%
    dplyr::mutate(
      variable = toupper(!!flag)
    ) %>%
    dplyr::select(
      variable
    ) %>%
    tidyr::separate(
      variable,
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
      -Names
    ) %>%
    dplyr::mutate(
      Words = str_replace_all(Words, '\\.COM', ''),
      Words = str_replace_all(Words, "[^[:alnum:]]", ""),
      Words = str_replace_all(Words, 'DO NOT', 'DONT'),
      Words = str_replace_all(Words, 'CAN NOT', 'CANT'),
      Words = str_replace_all(Words, 'CANNOT', 'CANT')
    ) %>%
    y2clerk::freqs(
      Words
    ) %>%
    wc_filter() %>%
    dplyr::filter(
      label != ''
    ) %>%
    dplyr::arrange(
      n %>% desc
    ) %>%
    dplyr::slice(
      1:top_x
    ) %>%
    dplyr::mutate(
      total =  dataset %>%
        dplyr::filter(
          !is.na(!!flag),
          !!flag != ''
        ) %>%
        dplyr::count() %>%
        as.numeric(),
      result = n / total,
      result = round(result, 2)
    ) %>%
    dplyr::select(
      - total
    )
}

#### wc_filter ####
wc_filter <- function(dataset){
  dataset %>%
    filter(
      !label %in% c(
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
