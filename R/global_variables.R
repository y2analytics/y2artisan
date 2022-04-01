### Global variables:
# These are objects used as defaults in functions,
# but these global objects should be created in the individual R session by the user.
# The default shortcuts are just there to making coding faster.
utils::globalVariables(
  c('frequencies',
    'CHART_PATH',
    'DATA_PATH',
    'doc',
    'color_settings',
    'color_settings_grouped',
    'color_settings_stacked',
    'size_settings',
    'text_settings',
    'text_settings_grouped',
    'text_settings_stacked',
    '.'
  )
)
