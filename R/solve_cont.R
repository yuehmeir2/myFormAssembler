
#' to solve an initialized lp object iteratively
#'
#' @param lpObj
#' the LP object that has been initialized and is ready to be solved.
#'
#' @param block_within_forms default is T.
#'
#' @param block_across_forms default is T.
#'
#' @param maxOverlapRate the maximum overlap rate between the new solution and the blocked solution.
#' The maximum number of overlapping items is a rounded integer of maxOverlapRate * number of items per form.
#'
#' @param max_usage the maximum item usage in maxIter
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
#' @param maxIter
#' maximum number of iterations
#'
#' @param exist_n_iter the number of existing iterations
#'
#' @param verbose
#'
#' @return
#' the solved lp object; if forms are successfully built, they can be found from formList,
#' consSummaryList, and formSummaryList under lp_obj.
#'
#' @export
#' @examples
#'   example_name = "MST08_tiny"
#'   solver = "cbc"
#'   timeout = 60
#'   gap = 0
#'   verbose = TRUE
#'   maxIter = 4
#'   maxOverlapRate = .50
#'   lp_obj2 = myFormAssembler::run_example(example_name, T, timeout, solver, gap, verbose)
#'   lp_obj2 = to_solve_cont(lp_obj2, block_within_forms = T, block_across_forms = T, maxOverlapRate = maxOverlapRate, timeout = timeout, solver = solver, maxIter = maxIter, gap = gap)


to_solve_cont <- function (
  lp_obj, block_within_forms = T, block_across_forms = T, maxOverlapRate = 0.8, max_usage = 1.0,
  timeout = NULL, solver = NULL, gap = NULL, maxIter = 10, exist_n_iter = 0, verbose = T, ...) {

  if (is.null(timeout)) timeout = lp_obj$options$timeout
  if (is.null(solver)) solver = lp_obj$options$solver
  if (is.null(gap)) gap = lp_obj$options$gap

  # continually solve until maxIter has reached or no solution found

  iter = exist_n_iter

  while (T) {

    # a. add iteration by 1

    iter = iter + 1
    print(iter)

    # saveRDS(lp_obj, file = "~/Documents/simulator/ATA_bundles/math/path_bp8_mst13_tcc_tif_enemy/lp_obj_temp.rds")
    # lp_obj = readRDS(file = "~/Documents/simulator/ATA_bundles/math/path_bp8_mst13_tcc_tif_enemy/lp_obj_temp_stophere.rds")

    if (iter > maxIter + exist_n_iter) break()

    if (verbose && iter %% 20 == 1) print(str_c("Iterations: ", iter - exist_n_iter))

    # b. add max-overlap-with-previous-solutions constraint

    if ((block_within_forms || block_across_forms) && maxOverlapRate < 1.0 && iter != 1)
      lp_obj <- addon_blockSolution (lp_obj, block_within_forms = block_within_forms,
                                     block_across_forms = block_across_forms, maxOverlapRate = maxOverlapRate)

    # c.1. has max_IE_rate objective

    if ("MAX_IE_RATE" %in% toupper(lp_obj$objective)) {

      lp_obj = modify_objective_for_Overall_max_IE_rate(lp_obj)
    }

    # c.2. has max_usage constraint

    if (max_usage < 1.0) {

      lp_obj = addon_accumulative_max_usage (lp_obj, max_usage_count = round(max_usage * maxIter, 0))

    }

    # d. has mutator ? (the last constriants being modified)

    if (!is.null(lp_obj$constraint$mutator)) {
      lp_obj = mutate_lp_constraint(lp_obj)
    }

    # e. solve it

    lp_obj <- solve_lp(lp_obj, timeout = timeout, solver = solver, gap = gap, verbose = verbose, ...)

    # for debugging
    # saveRDS(lp_obj, file = "lp_obj.rds")

    # f. add form result to lp_obj$formList etc.

    if (tail(lp_obj$lp$log$event, 1) == "new solution") {

      lp_obj <- get_forms(lp_obj)
      lp_obj <- get_form_summary(lp_obj)
      lp_obj <- get_constraint_summary(lp_obj)

      lp_obj$formList[[iter]] = lp_obj$output$form
      lp_obj$formSummaryList[[iter]] = lp_obj$output$form_summary
      lp_obj$consSummaryList[[iter]] = lp_obj$constraint_summary
    } else {

      saveRDS(lp_obj, file = "not_solveable_withintime_lp_obj.rds")
      break()
    }

  }

  return(invisible(lp_obj))

}
