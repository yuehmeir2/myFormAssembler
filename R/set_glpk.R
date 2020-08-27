#' Set GLPK linear program model object
#'
#' Set up a new GLPK linear program model object from a corresponding
#' R object.
#' @param lp
#' An R object that encapsulates the data used to specify a linear program model.
#' This is a list with the following components:
#' constraint, objective, var.type, upper and lower. The last three components
#' are optional and default to NULL.
#'
#' @return
#' A glpkAPI::glpkPtr object.
#'
#' @export


set_glpk <- function (lp) {

    suppressMessages(require(glpkAPI))

    num.con = length(lp$constraint)
    num.var = length(lp$objective)
    lp.glpk = initProbGLPK()
    addRowsGLPK(lp.glpk, num.con)
    addColsGLPK(lp.glpk, num.var)

    for (i in seq(num.con)) {

        con = lp$constraint[[i]]

        v = con$variable

        b = con$rhs

        coef = con$coef

        type = switch(con$type,
                      ">=" = GLP_LO,
                      "<=" = GLP_UP,
                      "="  = GLP_FX)

        setMatRowGLPK(lp.glpk, i, length(v), v, coef)

        setRowBndGLPK(lp.glpk, i, type, b, b)
    }

    for (j in seq(num.var)) {

        lower = lp$lower[j]
        upper = lp$upper[j]

        coef = lp$objective[j]

        var.type = switch(lp$var.type[j],
                      "binary"  = GLP_BV,
                      "integer" = GLP_IV,
                      "real"    = GLP_CV)

        if (is.na(lower)) {
          if (is.na(upper)) {
            bound.type = GLP_FR # no bounds, free variable
          }
          else {
            bound.type = GLP_UB # upper bound only
          }
        }
        else {
          if (is.na(upper)) {
            bound.type = GLP_LO # lower bound only
          }
          else {
            if (lower == upper) {
              bound.type = GLP_FX # lower == upper, fixed variable
            }
            else {
              bound.type = GLP_DB # lower and upper, double bounded variable
            }
          }
        }

        setObjCoefGLPK(lp.glpk, j, coef)

        setColKindGLPK(lp.glpk, j, var.type)

        setColBndGLPK(lp.glpk, j, bound.type, lower, upper)
    }

    return (lp.glpk)
}



