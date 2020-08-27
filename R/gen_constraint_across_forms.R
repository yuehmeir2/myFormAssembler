
#' Constraint across forms
#'
#' @return
#' A list of constraints

gen_constraint_across_forms <- function (forms, n.items, x, x.coef, y = NULL, y.coef = NULL,
                                     type, rhs, label = "", consId = NULL, ys = NULL, ys.coef = NULL) {

    x = outer(x, n.items * (forms - 1), "+")

    x = as.vector(x)

    num.forms = length(forms)

    x.coef = rep(x.coef, num.forms) [1:length(x)]

    constraint = list(
      label    = label,
      consId   = consId,
      variable = c(x, ys, y),
      coef     = c(x.coef, ys.coef, y.coef),
      type     = type,
      rhs      = rhs)

    return (list(constraint))
}

gen_psgcount_constraint_across_forms <- function (forms, n.psgs, ys.start.at, ys, ys.coef, type,
                                                  rhs, label = "", consId = NULL) {

  ys0 = outer(ys, n.psgs * (forms - 1), "+")
  ys0 = as.vector(ys0)
  n.forms = length(forms)
  ys0.coef = rep(ys.coef, n.forms)

  constraint = list(
      label    = label,
      consId   = consId,
      variable = ys0,
      coef     = ys0.coef,
      type     = type,
      rhs      = rhs)

  return (list(constraint))

}


