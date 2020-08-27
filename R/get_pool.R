

get_pool_IRT_PAR_col = function (alias, pool_col_names) {

  rows_irt_par = grep("^irt_par", alias$attribute)
  which_cols = unlist(lapply(alias$alias[rows_irt_par], function (i) which( pool_col_names %in% i)))

  return (which_cols)

}
