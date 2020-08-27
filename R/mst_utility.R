#' get mst path from the content table
#'
#' @param contentTbl
#' the content table
#'
#' @param n_module_in_a_test
#' number of modules to form a MST test
#'
#' @return
#' The updated LP object
#'
#' @export

get_mst_path_from_content = function (contentTbl, n_module_in_a_test = 0) {

  if (!("FORM_IND" %in% names(contentTbl))) # add FORM_IND if it has not exist yet
    contentTbl <- contentTbl %>%
      add_column(
        FORM_IND = lapply(.$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
        .before = 1) %>% dplyr::select(-FORM)

  if (any(is.na(contentTbl$SCOPE))) contentTbl$SCOPE[is.na(contentTbl$SCOPE)] = "WITHIN"
  # need to guess what is n_module_in_a_test if not provided
  if (n_module_in_a_test <= 0) {
    n_module_in_a_test <-
      contentTbl$FORM_IND[
        contentTbl$SCOPE == "ACROSS" & contentTbl$ATTRIBUTE != "_OVERLAP"] %>%
      lapply(., length) %>% unique() %>% unlist() %>% min

    if (is.null(n_module_in_a_test) || length(n_module_in_a_test) > 1 || length(n_module_in_a_test) <= 0) return ()
  }

  path <- contentTbl$FORM_IND[
    sapply(contentTbl$FORM_IND, length) == n_module_in_a_test &
      contentTbl$SCOPE != "WITHIN" & contentTbl$ATTRIBUTE != "_OVERLAP" ] %>% unique()

  return (path)
}


#' get number of panels from the content table
#'
#' @param contentTbl
#' the content table
#'
#' @param n_module_in_a_test
#' number of modules to form a MST test
#'
#' @return
#' The updated LP object
#'
#' @export
#'
get_n_panel_from_content = function (contentTbl, n_module_in_a_test = 0) {

  path = get_mst_path_from_content(contentTbl, n_module_in_a_test)

  stage1 = unique(sapply(path, "[[", 1))

  if (length(stage1) == 1) return (1)

  dif = rep(0, length(stage1)-1)

  for (i in 2:length(stage1))
    dif[i-1] = (stage1[i] - stage1[i-1])

  return (sum(dif > 1))

}

#' createMSTResearchRDS is to take items, content, alias, tic, tcc from an lp_obj and save to a rds
#' file that the CATSuite can read.
#'
#' @param lpObj
#' the LP object
#'
#' @param rds_name
#' the rds name to be saved
#'
#' @export

createMSTResearchRDS = function (lp_obj, rds_name) {

  to_save_rds_path = "~/Documents/MST/myFormAssembler/inst/rdsdata"

  lp_obj$alias <- aliasListToTibble (lp_obj$alias)

  lp_obj$constraint$content <-
    lp_obj$constraint$content %>%
    mutate(FORM = unlist(lapply(lp_obj$constraint$content$FORM_IND, paste0, collapse = ",")),
           FORM_IND = NULL) %>%
    select(CONS_ID, FORM, everything())

  # for tcc: add FORM and drop FROM_IND

  if (!is.null(lp_obj$constraint$tcc))
    lp_obj$constraint$tcc <-
    lp_obj$constraint$tcc %>%
    mutate(FORM = unlist(lapply(lp_obj$constraint$tcc$FORM_IND, paste0, collapse = ",")),
           FORM_IND = NULL) %>%
    select(FORM, everything())

  if (!is.null(lp_obj$constraint$tif))
    lp_obj$constraint$tif <-
    lp_obj$constraint$tif %>%
    mutate(FORM = unlist(lapply(lp_obj$constraint$tif$FORM_IND, paste0, collapse = ",")),
           FORM_IND = NULL) %>%
    select(FORM, everything())

  x = list(item = lp_obj$items, alias = lp_obj$alias,
           constraint = lp_obj$constraint$content,
           tcc = lp_obj$constraint$tcc,
           tif = lp_obj$constraint$tif)
  x <- Filter(Negate(is.null), x)
  write_rds(x = x,
            path = paste0(to_save_rds_path, "/", rds_name, ".rds"), compress = "gz")
}

#' generate_mst_all_path is to generate all possible MST paths for a given MST design stage design
#' file that the CATSuite can read.
#'
#' @param design
#' a string with "-" as seperater. e.g. 2-3-4
#' @return a data.frame with Stages and Paths
#' @export
generate_mst_all_path = function (design = "") {

  if (design == "") return ()

  n_module_per_stage = strsplit(design, "-")  %>% unlist() %>% as.integer()

  modules = list()
  n_modules = 0
  for (i in 1:length(n_module_per_stage)) {
    modules [[i]] = {1:n_module_per_stage[i] + n_modules}
    n_modules = n_modules + length(modules[[i]])
  }

  all_path = expand.grid(modules) %>% set_names(paste0("Stage", 1:length(n_module_per_stage)))
  rownames(all_path) = paste0("Path--", do.call("paste", c(all_path, sep = ",")))
  # > all_path
  # Stage1 Stage2 Stage3
  # Path--1,3,6      1      3      6
  # Path--2,3,6      2      3      6
  # Path--1,4,6      1      4      6
  return(all_path)
}
