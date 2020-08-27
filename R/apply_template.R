
#' Build the constraints using templates, which might include tcc, tif, bp, etc
#'
#' @param lp_obj the lp object contains the tcc file
#'
#' @return a LP object
#'

apply_template = function (lp_obj) {

  if (!is.null(lp_obj$template$tcc_template) && !is.null(lp_obj$use_template$tcc_template))
    lp_obj = apply_tcc_template(lp_obj)

  return (lp_obj)

}



apply_tcc_template = function (lp_obj) {

  if (is.null(lp_obj$template$tcc_template) || is.null(lp_obj$use_template$tcc_template))
    return(lp_obj)

  tcc_template = lp_obj$template$tcc_template
  use_tcc_template = lp_obj$use_template$tcc_template

  tcc = if (!is.null(lp_obj$constraint$tcc)) lp_obj$constraint$tcc else
             get_empty_IRT_fun_tibble ("TCC")

  for (i in seq_along(use_tcc_template)) {

    tcc_template_name = names(use_tcc_template[i])
    the_template = tcc_template[[names(use_tcc_template[i])]]
    apply_to = use_tcc_template[[i]]

    for (j in seq(nrow(apply_to))) {
      n_row = nrow(the_template)
      temp = dplyr::bind_cols (
                 tibble(FORM_IND = rep(apply_to$FORM_IND[j], n_row),
                        TCC_ID = as.character((nrow(tcc)+1):(nrow(tcc)+n_row)),
                        SCOPE = rep(apply_to$SCOPE[[j]], n_row),
                        MARGIN = rep(0, n_row),
                        NOTE = rep(tcc_template_name, n_row)
                 ),

                 the_template)

      tcc <- dplyr::bind_rows(tcc, temp)

    }

  }

  lp_obj$constraint$tcc = tcc

  return (lp_obj)
}

apply_slice_template = function (lp_obj) {

  if (is.null(lp_obj$template$slice_template) || is.null(lp_obj$use_template$slice_template))
    return(lp_obj)

}


