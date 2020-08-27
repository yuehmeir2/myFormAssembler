

get_empty_main_slice = function () {

  cons_tbl = tibble(
    CONS_ID         = integer(),
    FORM            = character(),
    CLUSTER         = character(),
    ATTRIBUTE       = character(),
    SCOPE           = character(),
    ATTRIBUTE_VALUE = character(),
    ATTRIBUTE_LOWER = double(),
    ATTRIBUTE_UPPER = double(),
    SLICE_UNIT      = character(),
    SLICE_LOWER     = integer(),
    SLICE_UPPER     = integer(),
    MARGIN          = double(),
    NOTE            = character()
  )

  return (cons_tbl)
}
