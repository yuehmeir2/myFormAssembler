
#' Content Constraint within forms
#'
#' @return a list of constraints for each form
#'


gen_constraint_within_forms <- function (forms, n.items, x, x.coef,
                                      type, rhs, label = "", consId = NULL, y = NULL, y.coef = NULL, ys = NULL, ys.coef = NULL) {

    x = outer(x, n.items * (forms - 1), "+")

    n.forms = length(forms)

    constraint = vector(mode = "list", length = n.forms)

    # if (length(rhs) == 1) rhs = rep(rhs, n.forms)

    for (f in seq(n.forms)) {

      constraint[[f]] = list(
        label    = label,
        consId   = consId,
        variable = c(x[, f], ys, y),
        coef     = c(x.coef, ys.coef, y.coef),
        type     = type,
        rhs      = rhs)
    }

    return (constraint)
  }

gen_group_constraint_within_forms <- function (forms, n.items, n.psgs,
                                               x, x.coef,
                                         type, rhs, label = "", consId = NULL, ys = NULL, ys.coef = NULL) {

  x0 = outer(x, n.items * (forms - 1), "+")

  n.forms = length(forms)

  ys0 = outer(ys, n.psgs * (forms - 1), "+")

  constraint = vector(mode = "list", length = n.forms)

  if (length(rhs) == 1) rhs = rep(rhs, n.forms)

  for (f in seq(n.forms)) {

    constraint[[f]] = list(
      label    = label,
      consId   = consId,
      variable = c(x0[, f], ys0[,f]),
      coef     = c(x.coef, ys.coef),
      type     = type,
      rhs      = rhs[f])
  }

  return (constraint)
}

#' Passage count constraint within forms
#'
#' @return a list of constraints for each form
#'

gen_psgcount_constraint_within_forms <- function (forms, n.psgs, ys.start.at, ys, ys.coef, type,
                                                rhs, label = "", consId = NULL) {

  ys0 = outer(ys, n.psgs * (forms - 1), "+")

  n.forms = length(forms)

  constraint = vector(mode = "list", length = n.forms)

  for (f in seq(n.forms)) {

    constraint[[f]] = list(
      label    = label,
      consId   = consId,
      variable = ys0[, f],
      coef     = ys.coef,
      type     = type,
      rhs      = rhs)

  }

  return (constraint)

}
