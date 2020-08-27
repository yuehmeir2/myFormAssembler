
#' Generate form summary
#'
#' @description
#'
#' @export





get_form_summary <- function (lp_obj) {

    if (!("form" %in% names(lp_obj$output))) lp_obj <- get_forms(lp_obj)

    form = lp_obj$output$form

    items = lp_obj$items

    # create form2 with form_ind item_ind  item_id point

    used_cols = c("point", "irt_par_b")

    cols = unlist(lp_obj$alias[used_cols])

    form2 <- dplyr::bind_cols (form,
                      items[form$item_ind, cols])

    colnames(form2) <- c(colnames(form), used_cols)

    # create form_summary table

    mean_diff   = aggregate(irt_par_b ~ form_ind, data = form2, FUN = mean)

    sd_diff     = aggregate(irt_par_b ~ form_ind, data = form2, FUN = sd)

    min_diff    = aggregate(irt_par_b ~ form_ind, data = form2, FUN = min)

    max_diff    = aggregate(irt_par_b ~ form_ind, data = form2, FUN = max)

    dup         = duplicated2(form$item_ind)

    # dup_item_id = if (any(dup)) form$item_id[which(dup)] else

    form_summary = aggregate(cbind(item = 1,
                                   point,
                                   dup = dup
                                   # dup_item_id = form$item_id[which(duplicated2(form$item_ind))]
                                   ) ~ form_ind,
                             data = form2, FUN = sum)

    form_summary = bind_cols(form_summary,
                             mean_diff = mean_diff$irt_par_b,
                             sd_diff   = sd_diff$irt_par_b,
                             min_diff  = min_diff$irt_par_b,
                             max_diff  = max_diff$irt_par_b)

    colnames(form_summary) = c("form_ind", "n_item", "n_point", "n_dup", "mean_diff",
                               "sd_diff", "min_diff", "max_diff")

    if (any(dup)) {
      duplicated_form_item_id <- aggregate(cbind(dup_item_id =
                                                 sapply(1:length(dup), function(i) if (dup[i]) form2$item_id[i] else NA)) ~ form_ind,
                                         data = {form2 %>% select(form_ind,item_id)}, FUN = paste, collapse = ",")
      form_summary <- dplyr::left_join(form_summary, duplicated_form_item_id, by = "form_ind")
    }  else
       form_summary$dup_item_id = rep(NA, nrow(form_summary))


    if (is_psg_info_outputable(lp_obj)) {

      n_psg <- aggregate(psg_id ~ form_ind, data = form2, FUN = n_distinct)
      form_summary <- bind_cols(form_summary,
                                n_psg = n_psg$psg_id)

    }

    lp_obj$output$form_summary = as_tibble(form_summary)

    return(invisible(lp_obj))

  }


