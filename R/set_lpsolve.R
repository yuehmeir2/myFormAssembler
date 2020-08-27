#' Set lpSolve linear program model object
#'
#' Set up a new lpSolve linear program model object from a corresponding
#' R object.
#' @param lp
#' An R object that encapsulates the data used to specify a linear program model.
#' This is a list with the following components: constraint, objective, var.type,
#' upper and lower. The last three components are optional and default to NULL.
#'
#' @return
#' A lpSolveAPI::lpExtPtr object. This is an external pointer to a "lprec" structure
#' used by the lp_solve C API that underlies the lpSolveAPI package.
#'
#' @export



set_lpsolve <- function (lp) {

    suppressMessages(require(lpSolveAPI))

    num.con = length(lp$constraint)
    num.var = length(lp$objective)

    lprec = make.lp(num.con, num.var)

    row.add.mode(lprec, "on")

    for (i in seq(num.con))
    {
        con = lp$constraint[[i]]

        set.row(lprec, i, con$coef, con$variable)

        set.constr.type(lprec, con$type, i)

        set.rhs(lprec, con$rhs, i)
    }

    for (j in seq(num.var))
    {
        set.type(lprec, j, lp$var.type[j])
    }

    set.bounds(lprec, upper = lp$upper, lower = lp$lower)

    set.objfn(lprec, lp$objective)

    row.add.mode(lprec, "off")

    return (lprec)
}



