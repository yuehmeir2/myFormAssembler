
#' solve an initialized LP object
#'
#' @param lp_obj
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
#' The updated LP object with an "x" component, which is
#' a binary indicator matrix that defines the items picked for forms.
#'
#' @export

solve_lp <- function (lp_obj, timeout = NULL, solver = NULL, gap = NULL, verbose = T, ...) {

    if (is.null(timeout)) timeout = lp_obj$options$timeout
    if (is.null(solver)) solver = lp_obj$options$solver
    if (is.null(gap)) gap = lp_obj$options$gap

    if (!("lp" %in% names(lp_obj)))
    {
      message("The LP object does not have a lp component\n")

      return(invisible(lp_obj))
    }

    if (timeout == 0)
    {
      message("The solver is not called because the timeout parameter is 0\n")

      return(invisible(lp_obj))
    }

    # add solver.time to lp$log

    if (!is.null(lp_obj$lp$log) && !("solver.time" %in% names(lp_obj$lp$log)))
      lp_obj$lp$log <- lp_obj$lp$log %>% dplyr::mutate(solver.time = NA)

    # solve

    lp = do.call(solver,
                 list(lp_obj$lp,
                      timeout = timeout,
                      gap     = gap,
                      verbose = verbose,
                      ...))

    # write_lp (lp, file = "model0.lp")

    lp_obj$lp = lp

    if (tail(lp$log$event, 1) == "new solution")
    {
      # update the x matrix

      x = lp$x

      n.items = nrow(lp_obj$items)
      n.forms = lp_obj$test_inf$n_form

      x = x[seq(n.items * n.forms)]

      x = as.integer(round(x))

      x = matrix(x, nrow = n.items)

      colnames(x) = lp_obj$form_inf$form_name
      rownames(x) = paste("item", seq(n.items), sep = "")

      lp_obj$x = x

    } else {
      saveRDS(lp_obj, file = "not_solved_lp_obj.rds")
    }

    return(invisible(lp_obj))
}


#' solve an initialized LP object with multiple panels
#'
#'  The constraints are specified for one panel. Then, the constraints are extended
#'  internally based on num_panels.
#'
#' @param lpObj
#' the LP object that has been initialized and is ready to be solved.
#'
#' @param num_panels
#' the number of panels desired.
#'
#' @param timeout
#' default is 60 seconds
#'
#' @param solver
#' "cbc" (default), "symphony", "glpk" and "lpsolve".
#'
#' @param gap
#' absolute MIP gap with default = 0
#'
#' @param verbose
#'
#' @return
#' the solved lp object; if forms are successfully built, they can be found from formList,
#' consSummaryList, and formSummaryList under lp_obj.
#'
#' @export
#' @examples
#' example_name = "MST08_tiny"
#' num_panels = 15
#' solver = "cbc"
#' timeout = 60
#' gap = 0
#' verbose = TRUE
#' lp_obj = myFormAssembler::run_example(example_name, T, timeout, solver, gap, verbose)
#' lp_obj$output = NULL
#' lp_obj$x = NULL
#' lp_obj = to_solve_panels(lp_obj, num_panels, timeout, solver, gap, verbose, across_form_max_item_usage_count = 5)
#'

to_solve_panels <- function (lp_obj, num_panels, timeout = NULL, solver = NULL, gap = NULL, verbose = T,
                             across_form_max_item_usage_count = NULL, form_skip_usage_count = NULL, save_to_file = NULL) {

  require(tidyverse)
  # modify test_inf and form_inf based on num_panels

  lp_obj$test_inf$n_panel = num_panels
  lp_obj$test_inf$n_form = lp_obj$test_inf$n_form * num_panels

  # e.g.
  # test_name n_form n_panel
  # <chr>      <dbl>   <dbl>
  # 1 MST 1-3        8       2

  ori_form_ind = lp_obj$form_inf$form_ind

  form_inf = map(1:num_panels, ~{
    lp_obj$form_inf %>% mutate(form_name = str_c("P_", .x, "_", form_name)) %>%
      mutate(form_ind = form_ind + (.x-1)* nrow(.)) %>%
      mutate(panel_ind = .x, .before = 1)
  }) %>% bind_rows()
  lp_obj$form_inf = form_inf

  # expand the lp from 1 to num_panels

  panel_length = length(lp_obj$lp$objective)

  # expand variable vectors horizontally
  lp_obj$lp$objective = rep(lp_obj$lp$objective, num_panels)
  lp_obj$lp$var.type = rep(lp_obj$lp$var.type, num_panels)
  lp_obj$lp$lower = rep(lp_obj$lp$lower, num_panels)
  lp_obj$lp$upper = rep(lp_obj$lp$upper, num_panels)

  # expand constraint lists vertically
  orig_constraints = lp_obj$lp$constraint
  expanded_constraints = NULL
  for (panel in 1:num_panels) {
    # panel = 1
    expanded_constraints = c(expanded_constraints, lapply(orig_constraints, function(constraint) {
      # constraint = orig_constraints[[1]]
      constraint$label = paste0("panel", panel, "-", constraint$label)
      if (!is.null(constraint$consId)) {
        constraint$consId = paste0("panel", panel, "-", constraint$consId)
      }
      constraint$variable = constraint$variable + ((panel-1) * panel_length)
      return(constraint)
    }))
  }
  lp_obj$lp$constraint = expanded_constraints

  # add panel overlap constraints by adding across_form_max_item_usage_count across all panels

  if (!is.null(across_form_max_item_usage_count) && across_form_max_item_usage_count > 0 && across_form_max_item_usage_count < num_panels*2) {

    max_usage_count_controlled_form = setdiff(1:lp_obj$test_inf$n_form, form_skip_usage_count)

    panel_overlap_constraints = lapply(1:nrow(lp_obj$items), function(itemInd) {

      consVar = vapply(max_usage_count_controlled_form, function(formInd) {
        as.integer(itemInd + ((formInd - 1) * nrow(lp_obj$items)))
      }, as.integer(0))
      consCoef = rep(1, lp_obj$test_inf$n_form)

      constraint = list(
        label    = paste0("panel-overlap-", lp_obj$items$ITEM_ID[[itemInd]]),
        variable = consVar,
        coef     = consCoef,
        type     = "<=",
        rhs      = across_form_max_item_usage_count
      )
      return(constraint)
    })
    lp_obj$lp$constraint = c(lp_obj$lp$constraint, panel_overlap_constraints)
  }

  # remove any existing solutions
  lp_obj$lp$x = NULL

  # solve the problem

  lp_obj = solve_lp(lp_obj, timeout = timeout, solver = solver, gap = gap, verbose = verbose,
                    log_to_console = 1)

  if (tail(lp_obj$lp$log$event, 1) != "new solution") {
    warning("Not solvable")
    return(invisible(lp_obj))
  }

  # process and getting output

  # get lp_obj$output$form
  lp_obj <- get_forms(lp_obj)

  # get lp_obj$output$form_summary
  lp_obj <- get_form_summary(lp_obj)

  # get lp_obj$constraint_summary

  # change lp_obj$constraint for constraint summary (***)
  constraint = lp_obj$constraint
  n_form_per_panel = length(ori_form_ind)

  # expand content constraint to many panels
  content_cons = constraint$content

  new_content_cons <- map (1:nrow(content_cons), ~{

    which_row = .x
    if (is.na(content_cons$SCOPE[[which_row]]) || content_cons$SCOPE[[which_row]] != "ACROSS") {
      content_cons$FORM_IND[[which_row]] <<- map(content_cons$FORM_IND[[which_row]], ~{
        .x+(1:num_panels-1)*n_form_per_panel
      }) %>% unlist() %>% sort()
      content_cons[which_row,]
    } else {
      map(1:num_panels, ~{
        cons = content_cons %>% slice(which_row)
        cons$FORM_IND[[1]] = c(cons$FORM_IND[[1]]+(.x-1)*n_form_per_panel)
        cons$CONS_ID = str_c(cons$CONS_ID, "_P", .x)
        cons
      }) %>% bind_rows()
    }

  }) %>% bind_rows()

  lp_obj$constraint$content =  new_content_cons

  # get lp_obj$constraint_summary
  lp_obj <- get_constraint_summary(lp_obj)

  # get lp_obj$formList
  lp_obj$formList = lp_obj$output$form %>%
    left_join({lp_obj$form_inf %>% select(panel_ind, form_ind)}, by = "form_ind") %>%
    group_by(panel_ind) %>% group_split()

  lp_obj$formSummaryList = lp_obj$output$form_summary %>%
    left_join({lp_obj$form_inf %>% select(panel_ind, form_ind)}, by = "form_ind") %>%
    group_by(panel_ind) %>% group_split()

  # skip lp_obj$consSummaryList creation

  if (!is.null(save_to_file)) saveRDS(lp_obj, file = save_to_file)

  return(invisible(lp_obj))


}
