#' get input from R objects
#'
#' @param lpObj
#' the LP object
#'
#' @param input_tibble
#' the input tibbles
#'
#' @param add_default_alias
#' if true, the default alias is used
#'
#' @return
#' The updated LP object
#'
#' @export


get_input <- function (lp_obj, input_tibble, add_default_alias = FALSE, verbose = F) {

  if (verbose) {
    print("call get_input()")
    print("input_tibble")
    print(input_tibble)
  }

  # no matter lp_obj$files have files listed or not, this function will update
  # the lp_obj based on input_tibble

  # alias

  if (verbose) print("read_alias")

  if (!is.null(input_tibble$alias)) lp_obj <- read_alias(lp_obj, input_tibble$alias)

  if (add_default_alias) lp_obj$alias = get_def_alias (max_pts = 6)

  # read in pool

  if (verbose) print("read_pool")

  if (!is.null(input_tibble$item)) lp_obj <- read_pool(lp_obj, input_tibble$item)

  # read in content constraints

  if (verbose) print("read_constraint")

  if (!is.null(input_tibble$constraint)) lp_obj <- read_constraint(lp_obj, input_tibble$constraint)

  # read in slice constraints

  if (verbose) print("read_slice")

  if (!is.null(input_tibble$slice)) lp_obj <- read_slice(lp_obj, input_tibble$slice, verbose = verbose)

  # read in tif target constraint

  if (!is.null(input_tibble$tif)) {

    if (verbose) print("read_tif")
    lp_obj <- read_tif(lp_obj, input_tibble$tif)
  }

  # read in tcc target constraint

  if (!is.null(input_tibble$tcc)) {

    if (verbose) print("read_tcc")
    lp_obj <- read_tcc(lp_obj, input_tibble$tcc, verbose = verbose)
  }

  # read in mutator constraint

  if (!is.null(input_tibble$mutator)) {

    if (verbose) print("read_mutator")
    lp_obj <- read_mutator(lp_obj, input_tibble$mutator)
  }

  return(invisible(lp_obj))

}


