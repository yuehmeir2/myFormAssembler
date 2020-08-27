#' Generate forms based on LP results
#'
#' @export


get_forms <- function (lp_obj) {

    x = lp_obj[["x"]]

    if (is.null(x)) {

      message("The lp_obj object does not contain a solution\n")

      return(invisible(lp_obj))
    }

    items = lp_obj$items
    alias = lp_obj$alias

    # get form index and item index from x

    w = which(x == 1)

    form.index = col(x)[w]
    item.index = row(x)[w]
    item.id    = items[[alias$item_id]][item.index]

    # item.id

    # form contain form inds and item inds

    form = tibble::tibble(form_ind = as.integer(form.index),
                  item_ind = item.index,
                  item_id = item.id)

    # get passage id if $passage_index exists

    if (is_psg_info_outputable(lp_obj)) {
      form$psg_id= items[[alias$psg_id]][item.index]
    }

    lp_obj$output$form = form

    return(invisible(lp_obj))

  }

