#' Write an lp object to a file in CPLEX LP format.
#'
#' @export

write_lp <- function (lp, file) {

    lp_glpk = set_glpk(lp)
    if (length(grep(".lp", tolower(file))) > 0) glpkAPI::writeLPGLPK(lp_glpk, file) else
    if (length(grep(".mps", tolower(file))) > 0) glpkAPI::writeMPSGLPK(lp_glpk, fmt = GLP_MPS_FILE, fname=file) else
      stop(str_c("not supported model name--", file))

}
