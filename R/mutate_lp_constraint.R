
#' mutate LP constraints
#'
#' @description $constraint$mutator may operate on constraints with regular ATTRIBUTE or _CLUSTER.
#' This feature depends on the user to provide a careful designed mutator spec; otherwise, it may
#' cause no feasible solution.
#'
#' @param lp_obj the lp object contains test_inf and xlsx file for constraints
#'
#' @return a LP object
#'
#' @export
#'

mutate_lp_constraint = function (lp_obj) {

  require(dplyr)

  if (!("constraint" %in% names(lp_obj)))
  {
    stop("The lp object does not have a constraint data frame")
  }
  if (!("content" %in% names(lp_obj$constraint)))
  {
    stop("The lp object does not have a constraint data frame")
  }
  if (!("mutator" %in% names(lp_obj$constraint)))
  {
    stop("The lp object does not have a constraint data frame")
  }

  mutator_cons = lp_obj$constraint$mutator %>%
                   left_join(lp_obj$constraint$content %>% mutate(CONS_ID = as.character(CONS_ID)), by = "CONS_ID")

  # loop should exclude rows with values in CLUSTER
  # those rows will be handled as a group by rows with CLUSTER NA and ATTRIBUTE _CLUSTER
  loop_mutator_cons = mutator_cons %>% filter(is.na(CLUSTER))
  for (k in seq(nrow(loop_mutator_cons))) {

    # k = 1
    mutator = loop_mutator_cons[k,]

    # if (startsWith(mutator$ATTRIBUTE, "EVIDENCE_STATEMENT")) {
    #
    #   lp_obj = mutate_standalone_constraint(mutator, lp_obj)
    #
    # } else
    if (mutator$ATTRIBUTE == "_CLUSTER") {

      cluster_cons = mutator_cons %>% filter(CLUSTER == mutator$ATTRIBUTE_VALUE)
      lp_obj = mutate_cluster_constraint(mutator, cluster_cons, lp_obj)

    } else {

      # stop(paste0("Mutate does not support constraints with ATTRIBUTE '", mutator$ATTRIBUTE, "'"))
      lp_obj = mutate_standalone_constraint(mutator, lp_obj)

    }
  }

  return(invisible(lp_obj))
}

mutate_standalone_constraint = function (mutator, lp_obj) {
  mutation_rhs = sample(mutator$SLICE_LOWER:mutator$SLICE_UPPER, 1)

  # find the lp_cons associated with the cons being mutated
  matching_lp_cons_ind = which(vapply(lp_obj$lp$constraint, function(lp_cons) {
    # lp_cons = lp_obj$lp$constraint[[1]]
    !is.null(lp_cons$consId) && (lp_cons$consId == mutator$CONS_ID)
  }, as.logical(0)))

  # update the rhs of each lp_cons associated with the cons being mutated
  for (lp_cons_ind in matching_lp_cons_ind) {
    # lp_cons_ind = matching_lp_cons_ind[[1]]
    message(paste0("Mutating standalone cons ", lp_obj$lp$constraint[[lp_cons_ind]]$consId, " ", lp_obj$lp$constraint[[lp_cons_ind]]$type, " from ", lp_obj$lp$constraint[[lp_cons_ind]]$rhs, " to ", mutation_rhs))
    lp_obj$lp$constraint[[lp_cons_ind]]$rhs = mutation_rhs
  }

  return(invisible(lp_obj))
}

mutate_cluster_constraint = function (mutator, cluster_cons, lp_obj) {
  # create a tibble with the base rhs of each cluster_cons
  mutation = tibble(
    CONS_ID = cluster_cons$CONS_ID,
    LOWER = as.integer(cluster_cons$SLICE_LOWER),
    UPPER = as.integer(cluster_cons$SLICE_UPPER),
    RHS = as.integer(cluster_cons$SLICE_LOWER)
  )

  # roll a random number of times, each time adding +1 to the rhs of a random cluster_cons
  n.rolls = seq(from = mutator$SLICE_LOWER, to = mutator$SLICE_UPPER)
  if (length(n.rolls) > 1) {
    n.rolls = sample(n.rolls, 1)
  }
  for (roll in seq_len(n.rolls)) {
    # only add +1 to cons that are below their max UPPER value
    eligible_ind = which(mutation$LOWER < mutation$UPPER)

    # add +1 to the rhs of a random cluster_cons
    if (length(eligible_ind) > 1) {
      selected_ind = sample(eligible_ind, 1, prob = cluster_cons$PROB[eligible_ind])
    } else if (length(eligible_ind) == 1) {
      selected_ind = eligible_ind
    } else {
      stop(paste0("invalid mutator: ", mutator$CONS_ID, ", eligible_ind: ", paste0(eligible_ind, collapse=",")))
    }
    mutation$RHS[[selected_ind]] = mutation$RHS[[selected_ind]] + 1
  }

  for (mutation_ind in seq(nrow(mutation))) {
    # mutation_ind = 1
    # find the lp_cons associated with the cons being mutated
    matching_lp_cons_ind = which(vapply(lp_obj$lp$constraint, function(lp_cons) {
      # lp_cons = lp_obj$lp$constraint[[1]]
      !is.null(lp_cons$consId) && (lp_cons$consId == cluster_cons$CONS_ID[[mutation_ind]])
    }, as.logical(0)))

    # update the rhs of each lp_cons associated with the cons being mutated
    for (lp_cons_ind in matching_lp_cons_ind) {
      # lp_cons_ind = matching_lp_cons_ind[[1]]
      message(paste0("Mutating cluster cons ", lp_obj$lp$constraint[[lp_cons_ind]]$consId, " ", lp_obj$lp$constraint[[lp_cons_ind]]$type, " from ", lp_obj$lp$constraint[[lp_cons_ind]]$rhs, " to ", mutation$RHS[[mutation_ind]]))
      lp_obj$lp$constraint[[lp_cons_ind]]$rhs = mutation$RHS[[mutation_ind]]
    }
  }

  return(invisible(lp_obj))
}
