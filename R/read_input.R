#' read in xlsx/xls files or google sheet files
#'
#' @param lp_obj
#' lp_obj$files might contain alias_file, pool_file, cons_file, slice_file,
#' passage_slice_file, tcc_file, tif_file
#'
#'
#' @return
#' The updated LP object
#'
#' @export


read_input <- function (lp_obj, add_default_alias = FALSE) {

  if (is.null(lp_obj$files)) {
    if (add_default_alias) lp_obj$alias = get_def_alias (max_pts = 6)
    return (lp_obj)
  }

  # read in alias

  if (!is.null(lp_obj$files$alias_file))
    lp_obj <- read_alias(lp_obj)

  if (add_default_alias) lp_obj$alias = get_def_alias (max_pts = 6)

  if (!is.null(lp_obj$files$pool_file))
    lp_obj <- read_pool(lp_obj)

  if (!is.null(lp_obj$files$cons_file)) lp_obj <- read_constraint(lp_obj)

  if (!is.null(lp_obj$files$mutator_file)) lp_obj <- read_mutator(lp_obj)

  if (!is.null(lp_obj$files$enemy_file)) lp_obj <- read_enemy(lp_obj)

  if (!is.null(lp_obj$files$slice_file)) lp_obj <- read_slice(lp_obj)

  if (!is.null(lp_obj$files$passage_slice_file)) lp_obj <- read_passage_slice(lp_obj)

  if (!is.null(lp_obj$files$tif_file)) lp_obj <- read_tif(lp_obj)

  if (!is.null(lp_obj$files$tcc_file)) lp_obj <- read_tcc(lp_obj)

  return(invisible(lp_obj))

}

trimSpace = function (str) {gsub("^\\s+|\\s+$", "", str)}

