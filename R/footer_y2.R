# Public function ---------------------------------------------------------
### footer_y2

#' Create a string with question wording and n-size
#'
#' Pipe in the dataset (responses) and specify variables. Save out the nicely formatted string to an object to pass to the footer of an ms chart.
#'
#' @keywords n question n-size footer
#' @param dataset Haven labelled dataset from which to pull the variable
#' @param ... Unquoted variables
#' @param q_type DEFAULT = 'NULL'; When 'NULL', footer_y2() will automatically choose the variable type when standard Y2 naming conventions are used. When variables have non-standard names, accepts list of any of c('s', 'm', 'oe', 'r', 'sl', 'md', 'n') used to specify question type. s = single select, m = multiple select, etc.
#' @param label_length DEFAULT = 15; Truncates labels to length 15 for groups and matrix questions and adds a '...' where truncation has occurred.
#' @param after_symbol DEFAULT = 'Select all that apply'; Removes all question text after and including the provided symbol.
#' @param prompt_rm DEFAULT TRUE; Attempts to better format the question by removing hard returns and removing the prompt for multi-select type questions. TRUE is needed in order to identify common stems.
#' @export
#' @examples
#' \dontrun{
#' responses %>%
#'   footer_y2(
#'    s_income
#'   )
#'
#' mtcars %>%
#'   footer_y2(
#'    m_aware,
#'    prompt_rm = TRUE
#'   )
#'   }

footer_y2 <- function(
  dataset,
  ...,
  q_type = 'NULL',
  label_length = 15,
  prompt_rm = TRUE,
  after_symbol = 'Select all that apply'
){


  # Error: Allow only once grouped data ------------------------------------

  if(
    dplyr::group_vars(dataset) %>% length() > 1
  ){

    stop('footer_y2 can currently only handle one grouping. Your data has multiple groups')

  }


  # Error: Grouping data has missing data -----------------------------------

  if(dplyr::is_grouped_df(dataset)){

    if(
      dataset %>%
      dplyr::select(
        .data[[dplyr::group_vars(dataset)]]
      ) %>%
      anyNA()
    ){

      stop('Grouping variable has missingness (NA\'s). Please manually filter out NA\'s or replace NA\'s with an explicit variable level.')

    }

  }




  # Make variable List ----------------------------------------------------

  variable_list <-
    dataset %>%
    dplyr::ungroup() %>%
    dplyr::select(
      ...
    ) %>%
    names()

  if(length(variable_list) == 0){

    variable_list <-
      dataset %>%
      dplyr::ungroup() %>%
      names()

  }


  # Check qtypes -------------------------------------------------------------

  q_type_original_specification <- q_type

  q_type <-
    check_qtypes(
      df = dataset,
      q.type = q_type,
      variables = variable_list
    )


  # Error: non standard naming ----------------------------------------------


  if(!all(q_type %in% c('s', 'm', 'r', 'n', 'sl', 'md', 'oe'))){

    stop(
      "Variable names do not match standardized format. Please specify q_types for all questions."
    )

  } else {

    if('NULL' %in% q_type_original_specification){

      message(
        'Note: Variable names match standardized format. Assuming question types.'
      )

    }

  }


  # Get the question text ---------------------------------------------------

  question <-
    get_questions(
      df = dataset,
      prompt.rm = prompt_rm,
      variables = variable_list,
      prompt.stem = after_symbol
    )


  # Error: Non-labelled data ------------------------------------------------



  for(i in 1:length(question)){

    if( question[i] == 'NULL' ){

      stop('variable "', variable_list[i],'is not haven labelled, please use labelled data with this function.')

    }

  }


  # Get common stems --------------------------------------------------


  stems <-
    get_stems(
      variables = variable_list,
      q.text = question,
      q.type = q_type
    )

  variable_list <-
    stems %>%
    dplyr::pull(.data$q_name)

  question <-
    stems %>%
    dplyr::pull(.data$q_text)

  q_type <-
    stems %>%
    dplyr::pull(.data$q_type)

  q_multi_stem <-
    stems %>%
    dplyr::pull(.data$q_multi_stem)

  q_stem <-
    stems %>%
    dplyr::pull(.data$q_stem)




  # Calculate n sizes -------------------------------------------------------


  # First loop --------------------------------------------------------------


  for(i in 1:length(variable_list)){

    if(i == 1){

      if( q_type[i] %in% c('s','n','oe','sl') & !dplyr::is_grouped_df(dataset) ){

        n <-
          singles_not_grouped(
            data = dataset,
            variable = variable_list[i],
            label.length = label_length,
            multi_stem = q_multi_stem[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('m', 'r', 'md') & !dplyr::is_grouped_df(dataset) ){

        n <-
          multi_not_grouped(
            data = dataset,
            variable = variable_list[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('s','n','oe','sl') & dplyr::is_grouped_df(dataset) ){

        n <-
          singles_grouped(
            data = dataset,
            variable = variable_list[i],
            label.length = label_length,
            multi_stem = q_multi_stem[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('m', 'r', 'md') & dplyr::is_grouped_df(dataset) ){

        n <-
          multi_grouped(
            data = dataset,
            variable = variable_list[i],
            q.stem = q_stem[i],
            label.length = label_length
          )

      }


      footer <-
        stringr::str_c(
          'Q: ',
          question[i],
          n,
          collapse = ''
        )


      # Second loop -------------------------------------------------------------



    } else {

      if( q_type[i] %in% c('s','n','oe','sl') & !dplyr::is_grouped_df(dataset) ){

        n <-
          singles_not_grouped(
            data = dataset,
            variable = variable_list[i],
            label.length = label_length,
            multi_stem = q_multi_stem[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('m', 'r', 'md') & !dplyr::is_grouped_df(dataset) ){

        n <-
          multi_not_grouped(
            data = dataset,
            variable = variable_list[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('s','n','oe','sl') & dplyr::is_grouped_df(dataset) ){

        n <-
          singles_grouped(
            data = dataset,
            variable = variable_list[i],
            label.length = label_length,
            multi_stem = q_multi_stem[i],
            q.stem = q_stem[i]
          )

      } else if( q_type[i] %in% c('m', 'r', 'md') & dplyr::is_grouped_df(dataset) ){

        n <-
          multi_grouped(
            data = dataset,
            variable = variable_list[i],
            q.stem = q_stem[i],
            label.length = label_length
          )

      }

      footer <-
        stringr::str_c(
          footer,
          '\nQ: ',
          question[i],
          n
        )

    }

  }

  footer

}

#' @rdname footer_y2
#' @export
ms_footer_y2 <- footer_y2

# Private Functions -------------------------------------------------------


### Singles not grouped
singles_not_grouped <- function(
  data,
  variable,
  label.length,
  multi_stem,
  q.stem
){

  if(stringr::str_detect(variable, '^oe_|_TEXT$')){

    data <-
      data %>%
      dplyr::filter(
        .data[[variable]] != ''
      )

  }

  if(multi_stem == FALSE){

    n <-
      data %>%
      dplyr::filter(
        !is.na(
          .data[[variable]]
        )
      ) %>%
      dplyr::count() %>%
      dplyr::pull(.data$n) %>%
      stringr::str_c(
        ' (n = ',
        .,
        ')'
      )

  } else {

    n <-
      data %>%
      dplyr::select(
        tidyselect::starts_with(
          q.stem
        ),
        -tidyselect::ends_with('_TEXT')
      ) %>%
      y2clerk::freq(
        prompt = TRUE,
        nas = FALSE
      ) %>%
      orderlabel::preamble_rm() %>%
      dplyr::group_by(
        .data$prompt
      ) %>%
      dplyr::add_tally(
        .data$n,
        name = 'new_n'
      ) %>%
      dplyr::distinct(
        .data$prompt,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        new_n = stringr::str_c(
          .data$prompt %>%
            stringr::str_trunc(
              width = label.length,
              side = 'right',
              ellipsis = '...'
            ) %>%
            stringr::str_trim(),
          ': ',
          .data$new_n
        )
      ) %>%
      dplyr::pull(.data$new_n) %>%
      stringr::str_flatten(', ') %>%
      stringr::str_c(
        ' n = (',
        .,
        ')'
      )

    message(
      stringr::str_c(
        'Note: Stem "',
        q.stem,
        '" was used to find n sizes.'
      )
    )

  }

  n

}


### Multi not grouped
multi_not_grouped <- function(
  data,
  variable,
  q.stem
){

  n <-
    data %>%
    dplyr::select(
      tidyselect::starts_with(
        q.stem
      ),
      -tidyselect::ends_with('_TEXT')
    ) %>%
    dplyr::mutate(
      ns = rowSums(
        dplyr::across(
          .cols = tidyselect::starts_with(q.stem),
          .fns = ~!is.na(.x)
        )
      )
    ) %>%
    dplyr::filter(
      .data$ns > 0
    ) %>%
    dplyr::count() %>%
    dplyr::pull(.data$n) %>%
    stringr::str_c(
      ' (n = ',
      .,
      ')'
    )


  message(
    stringr::str_c(
      'Note: Stem "',
      q.stem,
      '" was used to find n size.'
    )
  )

  n

}


### Singles grouped
singles_grouped <- function(
  data,
  variable,
  label.length,
  multi_stem,
  q.stem
){

  if(stringr::str_detect(variable, '^oe_|_TEXT$')){

    data <-
      data %>%
      dplyr::filter(
        .data[[variable]] != ''
      )

  }

  if(multi_stem == FALSE){

    n <-
      data %>%
      dplyr::filter(
        !is.na(
          .data[[variable]]
        )
      ) %>%
      dplyr::count() %>%
      forcats::as_factor() %>%
      dplyr::mutate(
        new_n = .data[[dplyr::group_vars(data)]] %>%
          as.character() %>%
          stringr::str_trunc(
            width = label.length,
            side = 'right',
            ellipsis = '...'
          ) %>%
          stringr::str_trim() %>%
          stringr::str_c(
            '(',
            .,
            ': ',
            n,
            ')'
          )
      ) %>%
      dplyr::pull(.data$new_n)  %>%
      stringr::str_flatten() %>%
      stringr::str_replace_all(
        '\\)\\(',
        ', '
      ) %>%
      stringr::str_c(
        ' n = ',
        .
      )

  } else {

    group_var_chars <-
      data %>%
      dplyr::count(
        dplyr::group_vars(data)
      ) %>%
      forcats::as_factor() %>%
      dplyr::pull(
        .data[[dplyr::group_vars(data)]]
      ) %>%
      as.character()


    for(i in 1:length(group_var_chars)){


      if(i == 1){

        n <-
          data %>%
          dplyr::ungroup() %>%
          dplyr::filter(
            as.character(
              forcats::as_factor(
                .data[[dplyr::group_vars(data)]]
              )
            ) == group_var_chars[i]
          ) %>%
          dplyr::select(
            tidyselect::starts_with(
              q.stem
            ),
            -tidyselect::ends_with('_TEXT')
          ) %>%
          y2clerk::freq(
            prompt = TRUE,
            nas = FALSE
          ) %>%
          orderlabel::preamble_rm() %>%
          dplyr::group_by(
            .data$prompt
          ) %>%
          dplyr::add_tally(
            .data$n,
            name = 'new_n'
          ) %>%
          dplyr::distinct(
            .data$prompt,
            .keep_all = TRUE
          ) %>%
          dplyr::mutate(
            new_n = stringr::str_c(
              .data$prompt %>%
                stringr::str_trunc(
                  width = label.length,
                  side = 'right',
                  ellipsis = '...'
                ) %>%
                stringr::str_trim(),
              ': ',
              .data$new_n
            )
          ) %>%
          dplyr::pull(.data$new_n) %>%
          stringr::str_flatten(', ') %>%
          stringr::str_c(
            ' n = (\n',
            group_var_chars[i] %>%
              stringr::str_trunc(
                width = label.length,
                side = 'right',
                ellipsis = '...'
              ) %>%
              stringr::str_trim(),
            ' = [',
            .,
            ']'
          )

      } else {

        n <-
          data %>%
          dplyr::ungroup() %>%
          dplyr::filter(
            as.character(
              forcats::as_factor(
                .data[[dplyr::group_vars(data)]]
              )
            ) == group_var_chars[i]
          ) %>%
          dplyr::select(
            tidyselect::starts_with(
              q.stem
            ),
            -tidyselect::ends_with('_TEXT')
          ) %>%
          y2clerk::freq(
            prompt = TRUE,
            nas = FALSE
          ) %>%
          orderlabel::preamble_rm() %>%
          dplyr::group_by(
            .data$prompt
          ) %>%
          dplyr::add_tally(
            .data$n,
            name = 'new_n'
          ) %>%
          dplyr::distinct(
            .data$prompt,
            .keep_all = TRUE
          ) %>%
          dplyr::mutate(
            new_n = stringr::str_c(
              .data$prompt %>%
                stringr::str_trunc(
                  width = label.length,
                  side = 'right',
                  ellipsis = '...'
                ) %>%
                stringr::str_trim(),
              ': ',
              .data$new_n
            )
          ) %>%
          dplyr::pull(.data$new_n) %>%
          stringr::str_flatten(', ') %>%
          stringr::str_c(
            n,
            ',\n',
            group_var_chars[i] %>%
              stringr::str_trunc(
                width = label.length,
                side = 'right',
                ellipsis = '...'
              ) %>%
              stringr::str_trim(),
            ' = [',
            .,
            ']'
          )

      }




    }

    n <-
      stringr::str_c(
        n,
        ')'
      )

    message(
      stringr::str_c(
        'Note: Stem "',
        q.stem,
        '" was used to find n sizes.'
      )
    )

  }

  n

}


### Multi grouped

multi_grouped <- function(
  data,
  variable,
  q.stem,
  label.length
){

  n <-
    data %>%
    dplyr::ungroup() %>%
    dplyr::select(
      tidyselect::starts_with(
        q.stem
      ),
      .data[[dplyr::group_vars(data)]],
      -tidyselect::ends_with('_TEXT')
    ) %>%
    dplyr::mutate(
      ns = rowSums(
        dplyr::across(
          .cols = tidyselect::starts_with(q.stem),
          .fns = ~!is.na(.x)
        )
      )
    ) %>%
    dplyr::filter(
      .data$ns > 0
    ) %>%
    dplyr::count(
      .data[[dplyr::group_vars(data)]]
    ) %>%
    forcats::as_factor() %>%
    dplyr::mutate(
      new_n = stringr::str_c(
        .data[[dplyr::group_vars(data)]] %>%
          as.character() %>%
          stringr::str_trunc(
            width = label.length,
            side = 'right',
            ellipsis = '...'
          ) %>%
          stringr::str_trim(),
        ': ',
        n
      )
    ) %>%
    dplyr::pull(.data$new_n) %>%
    stringr::str_c(
      .,
      collapse = ', '
    ) %>%
    stringr::str_c(
      ' n = (',
      .,
      ')'
    )

  message(
    stringr::str_c(
      'Note: Stem "',
      q.stem,
      '" was used to find n size.'
    )
  )

  n

}




check_qtypes <- function(
  df,
  q.type,
  variables
){

  if('NULL' %in% q.type){

    q_type_check <-
      df %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tidyselect::all_of(variables)
      ) %>%
      names() %>%
      stringr::str_split('_') %>%
      purrr::map(
        purrr::pluck(1)
      ) %>%
      as.vector()


    q.type <-
      q_type_check

  }

  q.type

}


get_questions <- function(
  df,
  prompt.rm,
  variables,
  prompt.stem
){

  if(prompt.rm == FALSE){

    question <-
      df %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tidyselect::all_of(variables)
      ) %>%
      labelled::var_label() %>%
      stringr::str_replace_all('\n', ' ') %>%
      stringr::str_squish()

  } else {

    question <-
      df %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tidyselect::all_of(variables)
      ) %>%
      labelled::var_label() %>%
      stringr::str_replace_all('\n', ' ') %>%
      stringr::str_remove(' - .+') %>%
      stringr::str_remove(
        stringr::str_c(
          prompt.stem,
          '.+')
      ) %>%
      stringr::str_trim() %>%
      stringr::str_squish()

  }

  question

}


get_stems <- function(
  variables,
  q.text,
  q.type
){

  possible_stems <-
    tibble::tibble(
      q_stem = variables %>%
        stringr::str_remove(
          '_[0-9]+$'
        ),
      q_num = variables %>%
        stringr::str_extract(
          '_[0-9]+$'
        ),
      q_text = q.text,
      q_type = q.type
    )

  known_stems <-
    dplyr::full_join(
      possible_stems %>%
        dplyr::filter(
          !is.na(.data$q_num)
        ) %>%
        dplyr::distinct(
          .data$q_stem,
          .data$q_text,
          .keep_all = TRUE
        ) %>%
        dplyr::group_by(.data$q_stem) %>%
        dplyr::add_count(
          name = 'false_stems'
        ) %>%
        dplyr::mutate(
          q_multi_stem = dplyr::case_when(
            false_stems == 1 ~ TRUE,
            false_stems > 1 ~ FALSE
          )
        ),
      possible_stems %>%
        dplyr::filter(
          is.na(.data$q_num)
        ) %>%
        dplyr::mutate(
          q_multi_stem = FALSE
        ),
      by = c("q_stem", "q_num", "q_text", 'q_type', 'q_multi_stem')
    ) %>%
    dplyr::mutate(
      q_type = .data$q_type %>% as.character(),
      q_name = dplyr::case_when(
        !is.na(.data$q_num) ~ stringr::str_c(.data$q_stem, .data$q_num),
        TRUE ~ .data$q_stem
      ),
      q_type = dplyr::if_else(
        stringr::str_detect(.data$q_stem, '_TEXT$'),
        'oe',
        .data$q_type
      )
    )


  known_stems

}




