#' Solve a linear program using GLPK.
#'
#' @param lp
#' An R object that encapsulates the data used to specify a
#' linear program
#'
#' @param timeout
#' The number of seconds before the solver must timeout
#'
#' @param gap.ratio
#' relative MIP gap tolerance
#'
#' @param verbose
#' Verbosity level
#'
#' @return
#' Updated lp object
#'
#' @export



glpk <- function (lp, timeout = 60, gap.ratio = 0, verbose = FALSE, ...) {

    suppressMessages(require(glpkAPI))

    lp.glpk = set_glpk(lp)

    setSimplexParmGLPK(TM_LIM, timeout * 1000)
    setMIPParmGLPK(TM_LIM, timeout * 1000)
    setMIPParmGLPK(MIP_GAP, gap.ratio)

    setSimplexParmGLPK(OUT_FRQ, 1000)

    if (verbose == FALSE)
    {
        setSimplexParmGLPK(MSG_LEV, GLP_MSG_OFF)
        setMIPParmGLPK(MSG_LEV, GLP_MSG_OFF)
    }

    status.code = solveSimplexGLPK(lp.glpk)

    if (status.code == 0)
    {
        status.code = solveMIPGLPK(lp.glpk)
    }

    status = switch(as.character(status.code),
                    "0" = "Optimal solution found",
                    "1" = "Invalid basis",
                    "2" = "Singular matrix",
                    "3" = "Ill-conditioned matrix",
                    "4" = "Invalid bounds",
                    "5" = "Solver failed",
                    "6" = "Objective lower limit reached",
                    "7" = "Objective upper limit reached",
                    "8" = "Iteration limit exceeded",
                    "9" = "Time limit exceeded",
                    "10" = "No primal feasible solution",
                    "11" = "No dual feasible solution",
                    "12" = "Root LP optimum not provided",
                    "13" = "Search terminated by application",
                    "14" = "Relative gap tolerance reached")

    if (status.code %in% c(0, 14))
    {
        event = "new solution"

        x = mipColsValGLPK(lp.glpk)

        lp$x = x

        objective = sprintf("%.8f", sum(x * lp$objective))

        status = paste(status, "- objective value", objective)

        message(status)

        message("The solution was updated\n")
    }

    else
    {
        event = "solver call"

        message(status)

        message("The solution was not updated\n")
    }

    log = data.frame2(
          event = event,
          solver = "glpk",
          solver.status = status)

    lp$log = rbind(lp$log, log)

    return(invisible(lp))
}









