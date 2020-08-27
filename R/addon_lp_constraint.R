
#' add special LP constraints to an existing LP object--pair overlap control
#'
#' @param lp_obj the LP object that has been solved at least one time
#'
#' @param whichSolution whichSolutiion in the lp_obj$formList should be added as a constraint controlling the overlap
#' rate between this solution and those being generated iteratively.
#'
#' @param block_within_forms if TRUE, the overlap rate control per form is added as new constraints.
#'
#' @param block_across_forms if TRUE, the overlap rate control across forms is added as a new constraint.
#' block_within_forms and block_across_forms can both be TRUE.
#'
#' @param maxOverlapRate the maximum overlap rate between the next new solution and the previous solutions.
#'
#' @param assignSolutionWtZero assign the solution items to have objective coef to be zeros.
#'
#' @return a LP object
#'
#' @export

addon_blockSolution = function (lp_obj, whichSolution = NULL, block_within_forms = T, block_across_forms = F,
                                maxOverlapRate = .8, assignSolutionWtZero = T) {

  if (is.null(whichSolution))
    whichSolution = length(lp_obj$formList)

  if (whichSolution <= 0) {
    warning("No solution exists; therefore, no solution is blocked!")
    return()
  }

  lp_cons = lp_obj$lp$constraint
  n.forms = lp_obj$test_inf$n_form

  blockSolution = lp_obj$formList[[whichSolution]]
  all_items_ind = blockSolution$item_ind %>% unique() %>% sort()

  if (block_within_forms) {
   for (ind in unique(blockSolution$form_ind)) {

        this_form_items = blockSolution %>% filter(form_ind == ind)
        lp_cons[[length(lp_cons)+1]] = gen_constraint_within_forms(
          forms = ind,
          n.items = nrow(this_form_items),
          x = this_form_items$item_ind,
          x.coef = rep(1, length(this_form_items$item_ind)),
          type = "<=",
          rhs = round( length(this_form_items$item_ind) * maxOverlapRate, 0),
          label = str_c("block within forms solution ", whichSolution))[[1]]
    }
  }

  if (block_across_forms) {

    # Note that each form has all ind from all forms' solution
    # therefore, the overlap across two iteration might be always <= maxOverlapRate

    lp_cons[[length(lp_cons)+1]] = gen_constraint_across_forms (
      forms = 1:n.forms,
      n.items = nrow(lp_obj$items),
      x = all_items_ind,
      x.coef = rep(1, length(all_items_ind)),
      type = "<=",
      rhs = round(length(all_items_ind) * maxOverlapRate, 0),
      label = str_c("block across forms solution ", whichSolution))[[1]]

  }

  # assign the objective coef to be zero for those items in the solution

  if (assignSolutionWtZero && "MIN_ITEM_USAGE_WT" %in% toupper(lp_obj$objective)) {

    n.items = nrow(lp_obj$items)
    x = as.vector(outer(all_items_ind, n.items * (1:n.forms - 1), "+"))

    lp_obj$lp$objective[x] = 0

  }

  # assign new constraints to lp_obj

  lp_obj$lp$constraint = lp_cons

  # remove components derived from the previous lp object

  lp_obj$lp$x = NULL
  lp_obj$output = NULL

  return(invisible(lp_obj))

}

#' add special LP constraints to an existing LP object
#'
#' @param lp_obj the LP object that has been solved at least one time
#'
#' @return a LP object
#'
#' @export

addon_accumulative_max_usage = function (lp_obj, max_usage_count) {

  lp_cons = lp_obj$lp$constraint

  if (length(lp_obj$formList) >= max_usage_count) {

    items = lp_obj$items
    alias = lp_obj$alias
    n.forms = lp_obj$test_inf$n_form

    formList = lp_obj$formList %>% bind_rows(.id = "iter") %>% select(-form_ind) %>%
      group_by(iter) %>% group_map(~distinct(.x)) %>% bind_rows()

    over_usage_item_ind = formList %>% count(item_ind) %>% filter(n > max_usage_count) %>%
      pull(item_ind)
      # right_join(items %>% mutate(item_ind = 1:nrow(items))) %>% select(item_ind, alias$item_id, n) %>%
      # arrange(item_ind)

    if (length(over_usage_item_ind) > 0) {

      all_labels = map(lp_cons , "label") %>% unlist()
      if ("block over-used items" %in% all_labels)
        cons_ind = which(all_labels == "block over-used items") else
        cons_ind = length(lp_cons)+1

        lp_cons[[cons_ind]] = gen_constraint_across_forms (
          forms = 1:n.forms,
          n.items = nrow(lp_obj$items),
          x = over_usage_item_ind,
          x.coef = rep(1, length(over_usage_item_ind)),
          type = "<=",
          rhs = 0,
          label = "block over-used items")[[1]]
    }

  }

  # assign new constraints to lp_obj

  lp_obj$lp$constraint = lp_cons

  # remove components derived from the previous lp object

  lp_obj$lp$x = NULL
  lp_obj$output = NULL

  return(invisible(lp_obj))

}


#' Modify objective coefficients to control the maximum item exposure rate in a series of solves
#'
#' Two ways of calculating the item exposure rates: one is based on the number of previous solves
#' (i.e. panels being built so far), and another way is use the total number of solutions that
#' the series of solves will achieve.
#' Two parameters are needed--lp_obj$objective_parameter$max_IE_rate and
#' lp_obj$objective_parameter$IE_rate_denominator (optional). If IE_rate_denominator = 0 or not
#' provided, the number of pervious solves will be used for calculating the item exposure rates.
#'
#' @param lp_obj the LP object that has been initialized and is ready to be solved.
#'
#' @return a LP object
#'
#' @export


modify_objective_for_Overall_max_IE_rate = function (lp_obj) {

  # get max_IE_rate and IE_rate_denominator

  if (is.null(lp_obj$objective_parameter$max_IE_rate) || lp_obj$objective_parameter$max_IE_rate <= 0 ||
      lp_obj$objective_parameter$max_IE_rate > 1.0) {
    stop("lp_obj$objective_parameter$max_IE_rate needs to be defined within (0,1]")
  }

  max_IE_rate = lp_obj$objective_parameter$max_IE_rate
  IE_rate_denominator = if (is.null(lp_obj$objective_parameter$IE_rate_denominator)) 0 else
    lp_obj$objective_parameter$IE_rate_denominator

  if (IE_rate_denominator <= 0) IE_rate_denominator = length(lp_obj$formList) + 1.0
  if (IE_rate_denominator < 1/max_IE_rate) return(invisible(lp_obj))

  # calculate current item exposure counts

  n.forms = lp_obj$test_inf$n_form
  alias = lp_obj$alias
  items = lp_obj$items
  n.items = nrow(items)

  exposed_item_count = lp_obj$formList %>% bind_rows %>% count(item_ind) %>%
    right_join(items %>% mutate(item_ind = 1:nrow(items))) %>% select(item_ind, alias$item_id, n) %>%
    arrange(item_ind)

  # calculate objective coef
  #   for the items with exposure rate < max_IE_rate, the coef = 0;
  #   otherwise, the coef are their exposure rates.

  updated_obj_coef <- map_dbl ((exposed_item_count$n+1.0)/(IE_rate_denominator) - max_IE_rate, ~{
    if (is.na(.x) || .x <=0) 0 else .x
  }) %>% unlist()

  if (lp_obj$options$verbose)
    print(str_c("past_n_iter = ", length(lp_obj$formList), " sum of coef = ", sum(updated_obj_coef)))

  objective = lp_obj$lp$objective

  x.form.len = n.items

  for (k in 1:n.forms) {
    # k = 1
    x.form.start = ((k - 1) * x.form.len) + 1
    x.ind = seq.int(from = x.form.start, length.out = n.items)

    # overwrite the x objective coefficients with
    objective[x.ind] = updated_obj_coef
  }

  # assign new constraints to lp_obj

  lp_obj$lp$objective = objective

  return(invisible(lp_obj))

}
