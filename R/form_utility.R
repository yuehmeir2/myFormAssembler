
is_psg_info_outputable = function (lp_obj) {

  if (!is.null(lp_obj$alias$psg_id))
    if (length(unique(lp_obj$items[[lp_obj$alias$psg_id]])) > 2 ) return (TRUE)
  return (FALSE)
}


#' form analysis
#'
#' @export



get_item_usage = function(lp_obj) {

  forms = lp_obj$formList %>% bind_rows(.id = "panel_ind")

  items = lp_obj$items

  item_id_alias = lp_obj$input_obj$alias$alias[which(lp_obj$input_obj$alias$attribute == "item_id")]

  item_IE_tbl = table(forms$item_id) %>% enframe() %>% set_names(item_id_alias, "FREQ") %>%
    mutate (FREQ = as.integer(FREQ)) %>% left_join(items)

  aa = item_IE_tbl %>% group_by(STANDARD2) %>% summarise(n = sum(FREQ))

}


