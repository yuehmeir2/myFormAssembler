
#' Build the tcc template to be used for a quick setup for tcc and easy understanding of tcc.
#'
#' @description
#' each tcc template contains a name and a tibble contains the columns THETA, TCC_LOWER,
#' and TCC_UPPER. If the tcc template name has existed, it will be overwritten.
#'
#' @param tcc_template the tcc template
#'
#' @param tcc_template_name
#' the name of this tcc template
#'
#' @param theta
#' the thetas
#'
#' @param tcc_lower
#' the tcc lower bound
#'
#' @param tcc_upper
#' the tcc upper bound
#'
#' @return a tibble of tcc template
#' @export

add_tcc_template = function (tcc_template = NULL, tcc_template_name, theta, tcc_lower, tcc_upper) {

  if (is.null(tcc_template)) {

    tcc_template = list()
    ind = 1

  } else { # add into existing tcc_template

    if (tcc_template_name %in% names(tcc_template)) {
      ind = which(names(tcc_template) == tcc_template_name)
      warning(paste0(tcc_template_name,  " is updated!"))
    } else
      ind = length(tcc_template) + 1

  }

  template = tibble (
    THETA = theta,
    TCC_LOWER = tcc_lower,
    TCC_UPPER = tcc_upper
  )

  tcc_template[[ind]] <- template
  names(tcc_template) [ind] = tcc_template_name

  return (tcc_template)

}

get_empty_IRT_fun_tibble = function (which_IRT_fun, with_one_NA_row = F) {

  # only for TCC, TIF, and CSEM
  if (toupper(which_IRT_fun) != "TCC" && toupper(which_IRT_fun) != "TIF" && toupper(which_IRT_fun) != "CSEM")
     return ()

  # create a tibble with 0 row
  df = tibble( ID = as.character(),
               FORM = as.character(),
               SCOPE = as.character(),
               THETA = as.double(),
               LOWER = as.double(),
               UPPER = as.double(),
               MARGIN = as.double(),
               NOTE = as.character())

  # add a row with NA
  if (with_one_NA_row)
    bind_rows(df, tibble(TCC_ID = NA))

  names(df)[c(1,5,6)] = paste0(toupper(which_IRT_fun), "_", names(df)[c(1,5,6)] )

  return (df)
}


add_slice_template = function (slice_template = NULL, slice_template_name, slice) {

  slice <- bind_rows(get_empty_main_slice(), slice)

  if (is.null(slice_template)) {
    slice_template = list()
    ind = 1

  } else { # add into existing slice_template

    if (slice_template_name %in% names(slice_template)) {

      warning(paste0(slice_template_name,  " is updated!"))
      ind = which(names(slice_template) == slice_template_name)

    } else

      ind = length(slice_template) + 1
  }

  slice_template[[ind]] <- slice
  names(slice_template) [ind] = slice_template_name

  return (slice_template)

}
