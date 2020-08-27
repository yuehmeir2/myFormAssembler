#' Solve a linear program using lpSolve.
#'
#' @param lp an R object that encapsulates the data used to specify a linear program model.
#'           This is a list with the following components: constraint, objective, var.type,
#'           upper and lower. The last three components are optional and default to NULL.
#'
#' @param timeout
#' either a positive integer value specifying the number of seconds after
#' which a timeout will occur, or zero, then no timeout will occur.
#'
#' @param presolve
#' a character vector specifying presolve steps to be carried out before solving
#'
#' @param verbose
#' either a character string or a TRUE/FALSE value to control the level
#' of reporting. The character string can be:
#'     "critical" (hard errors only),
#'     "severe" (errors only),
#'     "important" (warnings and errors),
#'     "normal",
#'     "detailed", or
#'     "full".
#'     TRUE means "normal", FALSE means "neutral".
#'
#' @param ...
#' more control parameters to be passed to lpSolve
#'
#' @return
#' Updated lp object
#'
#' @export



lpsolve <- function(lp, timeout = 10, presolve = c("rows", "cols"), gap = 0, verbose = FALSE, ...) {

    suppressMessages(require(lpSolveAPI))

    if (verbose == TRUE)
    {
        verbose = "normal"
    }

    if (verbose == FALSE)
    {
        verbose = "neutral"
    }

    mip.gap = c(gap, 0)

    lprec = set_lpsolve(lp)
    
    lp.control(lprec, timeout = timeout, presolve = presolve,
               mip.gap = mip.gap, verbose = verbose, ...)

    status.code = solve(lprec)

    status = switch(as.character(status.code),
                    "0" = "Optimal solution found",
                    "1" = "The model is sub-optimal",
                    "2" = "The model is infeasible",
                    "3" = "The model is unbounded",
                    "4" = "The model is degenerate",
                    "5" = "Numerical failure encountered",
                    "6" = "Process aborted",
                    "7" = "Optimization was stopped due to timeout",
                    "9" = "The model was solved by presolve",
                    "10" = "The branch and bound routine failed",
                    "11" = "The branch and bound was stopped because of a break-at-first or break-at-value",
                    "12" = "A feasible branch and bound solution was found",
                    "13" = "No feasible branch and bound solution was found"
                    )

    if (status.code %in% c(0, 1))
    {
        event = "new solution"

        x = get.primal.solution(lprec, orig = TRUE)

        x = x[(length(lp$constraint) + 1):length(x)]

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
          solver = "lpsolve",
          solver.status = status)

    lp$log = rbind(lp$log, log)

    return(invisible(lp))
}













