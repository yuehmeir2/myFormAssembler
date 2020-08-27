#' Solve a linear program using the cplex solver
#'
#' @param lp
#' An R object that encapsulates the data used to specify a
#' linear program
#'
#' @param timeout
#' The number of CPU seconds before the solver must timeout
#'
#' @param gap
#' Absolute MIP gap
#'
#' @param verbose
#' Verbosity level
#'
#' @return
#' Updated lp object
#'
#' @examples
#'   setwd("/Users/ychien/Documents/code/R/MST/myFormAssembler")
#'   lp = readRDS("inst/rdsdata/Ex02.rds")
#'   timeout = 60
#'   gap = 0
#'   verbose = FALSE
#'   solverCmd = "cplex"
#'   result = cplex(lp, timeout, gap, verbose, solverCmd)
#'
#' @import xml2
#' @export

cplex <- function (lp, timeout = 60, gap = 0, verbose = FALSE, solverCmd = NULL, ...) {

    require(xml2)

    if (verbose) verbose = 1 else verbose = 0

    # use the temporary directory to
    # write input and output files for cplex

    wd = setwd(tempdir())
    message("Running solver at ", getwd(), "\n")
    on.exit(setwd(wd))

    # remove temporary files from previous runs

    system("rm -f model.lp")
    system("rm -f x.cmd")
    system("rm -f x.sol")

    # write the linear program to a file in CPLEX LP format

    message("Processing MIP model object\n")

    write_lp(lp, "model.lp")

    # build the cplex command file

    # read the LP model from the model.lp file
    # set allowed absolute MIP gap
    # set verbose level
    # solve the LP
    # print result to the x file

    solverCmdOpts = c(
        "read model.lp",
        paste0("set timelimit ", timeout),
        paste0("mip tolerances mipgap ", max(gap, 0.01)),
        paste0("mip display ", verbose),
        "mipopt",
        "write x.sol"
    )

    writeLines(solverCmdOpts, "x.cmd")

    # run the cplex command

    if (is.null(solverCmd)) {
        solverCmd = "cplex"
    }
    solverCmdArgs = c("-f", "x.cmd")
    solverCmdResult = system2(solverCmd, solverCmdArgs, stdout = T, stderr = T, wait = T)

    # CPlex always returns a 0 status code.  Cannot use that value to determine if an error occurred.
    # Instead look for lines with "CPLEX Error" in the solverCmdResult
    errorLines = grep("CPLEX Error", solverCmdResult, value = T)
    if (length(errorLines) > 0) {
        stop(paste(errorLines, collapse = "\t"))
        return(NULL)
    }

    # process output file

    status = grep("solution:  Objective", solverCmdResult, value = T)
    status = ifelse(length(status) < 1, NULL, status[[length(status)]])

    message(status)

    # TODO: Update to handle mipstart, and use 'else' if solution not updated
    if (grepl("optimal", status))
    {
        event = "new solution"
        message("Get new solution!")

        solutionXML = xml2::as_list(xml2::read_xml("x.sol"))
        solutionVars = solutionXML$CPLEXSolution$variables
        x = vapply(solutionVars, function(var) {
            # "value" has type <chr>.  as.integer("0.9999") => 0, so we need to use round()
            as.integer(round(as.numeric(attr(var, "value"))))
        }, as.integer(0), USE.NAMES = FALSE)

        lp$x = x
        message("The solution was updated\n")
    }
    else
    {
        event = "solver call"
        message("The solution was not updated\n")
    }

    log = data.frame2(
          event = event,
          solver = "cplex",
          solver.status = status)

    lp$log = rbind(lp$log, log)

    return(invisible(lp))
}

