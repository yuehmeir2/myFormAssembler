
#' Get a list of default column aliases
#'
#' @param max_pts the number of irt_par_d columns that should be included
#' @return list of default column aliases
#'
#' @export

get_def_alias = function (max_pts = 6) {

  attribute = c("item_id", "task_type", "evidence_statement", "point",
                "irt_model", "irt_scale", "irt_par_a", "irt_par_b",
                "irt_par_c")

  alias = as.list(attribute)
  names(alias) = attribute

  return(alias)
}

