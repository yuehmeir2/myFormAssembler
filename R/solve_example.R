
#' to solve an initialized lp object
#'
#' @param lpObj
#' the LP object
#'
#' @param solver
#' "cbc" (default), "symphony", "glpk" and "lpsolve".
#'
#' @param timeout
#' default is 60 seconds
#'
#' @param gap
#' absolute MIP gap with default = 0
#'
#' @param verbose
#'
#' @return
#' the solved lp object
#'
#' @export

to_solve <- function (lp_obj, timeout = NULL, solver = NULL, gap = NULL, verbose = NULL) {

  # overwritten the initial setting if not NULL

  if (!is.null(solver))  lp_obj$options$solver = solver

  if (!is.null(timeout)) lp_obj$options$timeout = timeout

  if (!is.null(gap)) lp_obj$options$gap = gap

  if (!is.null(verbose))  lp_obj$options$verbose = verbose

  lp_obj <- add_lp_constraint(lp_obj)

  lp_obj <- solve_lp(lp_obj, timeout = timeout, solver = solver, gap = gap, verbose = verbose)

  if (tail(lp_obj$lp$log$event, 1) == "new solution") {

    lp_obj <- get_forms(lp_obj)

    lp_obj <- get_form_summary(lp_obj)

    lp_obj <- get_constraint_summary(lp_obj)

  }

  return(invisible(lp_obj))

}
