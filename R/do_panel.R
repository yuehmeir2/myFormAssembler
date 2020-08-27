
#' duplicate $constriant for panel and the $form_inf is modified accordingly
#' note that the $constraint$content must contain only one panel information in order to 
#' duplicate 
#'
#' @param lpObj
#' the LP object
#'
#' @return
#' The updated LP object
#'
#' @export

do_panel = function (lp_obj) {

  # the content table must specify only one panel if exists so 
  # this function can duplicate panels based on 
  # the number of panels specified through lp_obj$mst$n_panel
  
  content = NULL
  
  # The number of panel that can be found from the content must be 1
  if ("content" %in% names(lp_obj$constraint)) {
    
    content = lp_obj$constraint$content
    n_panel_in_content = get_n_panel_from_content(content)
   
    if (n_panel_in_content > 1) {
      warning("number of existing panels in content table is greater than 1! ")
      return (lp_obj)
    }
  }
  
  n_panel = lp_obj$mst$n_panel

  n_form = lp_obj$test_inf$n_form

  if (n_panel <= 1) return (lp_obj)

  # duplicate constraint

  # 1. duplicate constraint$content

  if (!is.null(content)) {

    n_content_row = nrow(content)

    
    for (p in 2:n_panel) {
      
      to_add_content = lp_obj$constraint$content

      # change FORM_IND for the panel p

      to_add_content$FORM_IND = lapply(to_add_content$FORM_IND, function (i)
        i = i + n_form * (p-1) )

      # change CONS_ID

      to_add_content$CONS_ID = ((p-1) * n_content_row + 1):(p * n_content_row)

      # change CLUSTER   (later)

      # change NOTE

      to_add_content$NOTE = paste0(to_add_content$NOTE, " P", p)

      content = bind_rows(content, to_add_content)

    }
  }

  # 2. duplicate constraint$tcc

  tcc = NULL

  if ("tcc" %in% names(lp_obj$constraint)) {

    tcc = lp_obj$constraint$tcc

    n_tcc_row = nrow(tcc)

    for (p in 2:n_panel) {

      to_add_tcc = lp_obj$constraint$tcc

      # change FORM_IND for the panel p

      to_add_tcc$FORM_IND = lapply(to_add_tcc$FORM_IND, function (i)
        i = i + n_form * (p-1) )

      # change TCC_ID

      to_add_tcc$TCC_ID = as.character(((p-1) * n_tcc_row + 1):(p * n_tcc_row))

      # change NOTE

      to_add_tcc$NOTE = paste0(to_add_tcc$NOTE, " P", p)

      tcc = bind_rows(tcc, to_add_tcc)

    }

  }

  # 3. duplicate constraint$tif

  tif = NULL

  if ("tif" %in% names(lp_obj$constraint)) {

    tif = lp_obj$constraint$tif

    n_tif_row = nrow(tif)

    for (p in 2:n_panel) {

      to_add_tif = lp_obj$constraint$tif

      # change FORM_IND for the panel p

      to_add_tif$FORM_IND = lapply(to_add_tif$FORM_IND, function (i)
        i = i + n_form * (p-1) )

      # change TIF_ID

      to_add_tif$TIF_ID = as.character(((p-1) * n_tif_row + 1):(p * n_tif_row))

      # change NOTE

      to_add_tif$NOTE = paste0(to_add_tif$NOTE, " P", p)

      tif = bind_rows(tif, to_add_tif)

    }

  }

  if (!is.null(content)) lp_obj$constraint$content = content
  if (!is.null(tcc))     lp_obj$constraint$tcc = tcc
  if (!is.null(tif))     lp_obj$constraint$tif = tif

  # modify $test_inf

  lp_obj$test_inf$n_form  = lp_obj$test_inf$n_form * n_panel
  lp_obj$test_inf$n_panel = n_panel

  # modify $form_inf

  form_inf = lp_obj$form_inf

  n_form_inf_row = nrow(form_inf)

  for (p in 2:n_panel) {

    to_add_form_inf = lp_obj$form_inf

    # change form_ind for the panel p

    to_add_form_inf$form_ind = ((p-1) * n_form_inf_row + 1):(p * n_form_inf_row)

    # change form_name

    to_add_form_inf$form_name = paste0(to_add_form_inf$form_name, "_P", p)

    form_inf = bind_rows(form_inf, to_add_form_inf)
  }

  lp_obj$form_inf = form_inf

  # modify mst if exists
  
  mst = lp_obj$mst 
  
  if (!is.null(mst)) {
    
    n_form_in_a_panel = lp_obj$test_inf$n_form / lp_obj$test_inf$n_panel
    
    if (!is.null(mst$path)) {
      
      for (p in 2:n_panel) {
        
        to_add_path = lp_obj$mst$path
        
        to_add_path = lapply(to_add_path, function (i) i+n_form_in_a_panel)
        
        mst$path = c(mst$path, to_add_path)
      }
      names(mst$path) = paste0("p", 1:length(mst$path))
    }
    
    if (!is.null(mst$overlap_form)) {
      for (p in 2:n_panel) {
        
        to_add_overlap_form = lp_obj$mst$overlap_form
        
        to_add_overlap_form = to_add_overlap_form + (n_form_in_a_panel) * (p-1)
        
        mst$overlap_form = c(mst$overlap_form, to_add_overlap_form)
      }
      
    }
    lp_obj$mst = mst
  }
  
  return (lp_obj)

}
