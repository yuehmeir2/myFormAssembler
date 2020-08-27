#' Solve a linear program using the cbc solver
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
#' @export


cbc <- function (lp, timeout = 60, gap = 0, verbose = T, ...) {

    require(stringr)

    if (verbose) verbose = 1 else verbose = 0

    # use the temporary directory to
    # write input and output files for cbc

    wd = setwd(tempdir())
    message("Running solver at ", getwd(), "\n")
    on.exit(setwd(wd))

    # remove temporary files from previous runs

    system("rm -f model.lp")
    system("rm -f x0")
    system("rm -f x")

    # write the linear program to a file in CPLEX LP format

    message("Processing MIP model object\n")

    write_lp(lp, "model.lp")

    # write the initial feasible solution (if any) to file

    if ("x" %in% names(lp))
    {
        message("Processing MIP starting value\n")

        value = as.vector(lp$x)

        index = paste("x", seq_along(value), sep = "_")

        x = data.frame(index, value)

        write.table(x, file = "x0", quote = FALSE)
    }

    # build the cbc command

    # read the LP model from the model.lp file
    # set allowed absolute MIP gap
    # set verbose level
    # solve the LP
    # print result to the x file

    cbc.cmd = paste(
                    "cbc model.lp",
                    "-cuts root",
                    "-cost priorities",
                    "-mipstart x0",
                    "-seconds", timeout,
                    # "-allow", gap + 0.01,
                    "-ratio", max(gap, 0.01),
                    "-log", verbose,
                    "-solve -solution x")

    # run the cbc command

    system(cbc.cmd)

    # process output file

    status = readLines("x", 1)

        message(status)

        if (substr(status, 1, 7) == "Optimal" || substr(status, 1, 15) == "Stopped on time")
        {

            message("Get new solution!")

            event = "new solution"

            x = read.table("x", skip = 1, stringsAsFactors = FALSE)

            x = x [which(x[,3] == 1),]

            index = as.numeric(str_remove(x[, 2], "[^_]*_"))  # remove anything before _ e.g. 23z_345 => 345

            value = x[,3]

            num.x = length(lp$objective)

            x = rep(0, num.x)

            names(x) = paste("x", seq(num.x), sep = "_")

            x[index] = value

            names(x) = NULL

            lp$x = x

            message("The solution was updated\n")
        }
        else
        {
            if (str_detect(status, "Stopped on iterations - objective value"))
                event = status else
                    event = "solver call"

        message("The solution was not updated\n")
    }

    log = data.frame2(
          event = event,
          solver = "cbc",
          solver.status = status,
          solver.time = 0)

    # lp$log = rbind(lp$log, log)
    if (!is.null(lp$log)) lp$log = rbind(lp$log, log) else lp$log = log
    return(invisible(lp))
}









