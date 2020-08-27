
#' Initialize a lpObj object
#'
#' @export


init_lp_obj <- function (basic, input = list(files = NULL, input_obj = NULL),
                         objective, template = NULL, use_template = NULL, mst = NULL) {

    require(tibble)

    lp_obj = list()

    lp_obj$files = input$files

    lp_obj$input_obj = input$input_obj

    lp_obj$job_id = basic$job_id

    lp_obj$test_inf =
      tibble(
        test_name = basic$test_name,
        n_form = as.integer(basic$n_form),
        n_panel = if (is.null(mst$n_panel)) integer(1) else mst$n_panel)

    if (is.null(basic$form_name)) basic$form_name = paste0("FORM_", 1:basic$n_form)
    lp_obj$form_inf =
      tibble(
        form_ind  = 1:basic$n_form,
        form_name = basic$form_name,
        n_item_l    = if (is.null(basic$n_item_l)) integer(1) else basic$n_item_l,
        n_item_u    = if (is.null(basic$n_item_u)) integer(1) else basic$n_item_u,
        n_point_l   = if (is.null(basic$n_point_l)) integer(1) else basic$n_point_l,
        n_point_u   = if (is.null(basic$n_point_u)) integer(1) else basic$n_point_u,
        relax     = as.integer(basic$relax))

    lp_obj$mst = mst

    lp_obj$template = template

    lp_obj$use_template = use_template

    lp_obj$objective = objective

    lp_obj$options = list(timeout = 0)

    lp_obj$output = list()

   return(invisible(lp_obj))
}

