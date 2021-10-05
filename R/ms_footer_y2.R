# Public function ---------------------------------------------------------
### ms_footer_y2

#' Create a string with question wording and n-size
#'
#' Pipe in the dataset (responses), specify the variable, and set the question type. Save out the nicely formatted string to an object to pass to the footer of an ms chart.
#'
#' @keywords n question n-size footer
#' @param dataset Haven labelled dataset from which to pull the variable
#' @param variable Unquoted variable
#' @param q_type DEFAULT = 's'; Can be any of c('s', 'm') used to specify question type. s = single select, m = multiple select, etc. Meant to match Y2 question naming conventions.
#' @export
#' @examples
#' \dontrun{
#' responses %>%
#'   ms_footer_y2(
#'    s_income
#'   )
#'
#' mtcars %>%
#'   ms_footer_y2(
#'    m_aware,
#'    q_type = 'm'
#'   )
#'   }

ms_footer_y2 <- function(
  dataset,
  variable,
  q_type = c('s', 'm')
){

  q_type <-
    rlang::arg_match(q_type)

  var_name <-
    dataset %>%
    dplyr::select(
      {{ variable }}
    ) %>%
    names() %>%
    stringr::str_split('_')

  if(var_name[[1]][1] != q_type){

    message(
      stringr::str_c(
        'Warning: Currently q_type = "',
        q_type,
        '". Please consider changing to q_type = "',
        var_name[[1]][1],
        '" because the variable name starts with "',
        var_name[[1]][1],
        '".'
      )
    )

  }

  question <-
    dataset %>%
    dplyr::select(
      {{ variable }}
    ) %>%
    labelled::var_label()

  if( question == 'NULL' ){

    stop('variable is not haven labelled, please use labelled data with this function.')

  }


  if( q_type == 's'){

    n <-
      dataset %>%
      dplyr::filter(
        !is.na({{ variable }})
      ) %>%
      dplyr::count() %>%
      dplyr::pull(n)

  } else if( q_type == 'm'){

    n <-
      dataset %>%
      dplyr::count() %>%
      dplyr::pull(n)

    message('NOTE: Be sure your dataset is filtered the same way as your original chart')

  }

  footer <-
    stringr::str_c(
      'Q: ',
      question,
      ' (n = ',
      n,
      ')'
    )

  footer

}
