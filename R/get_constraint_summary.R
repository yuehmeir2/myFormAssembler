
#' Analysis constraints in the form
#'
#' @param lp_obj the lp object
#'
#' @export

get_constraint_summary <- function (lp_obj) {
  require(purrr)

  form = lp_obj$output$form

  if (!("form_summary" %in% names(lp_obj$output)))
    form_summary = get_form_summary(lp_obj) else
    form_summary = lp_obj$output$form_summary

  form_items <- as.data.frame(bind_cols (form,
                      lp_obj$items[form$item_ind,]))

  form_items <- split.data.frame(form_items, f = form_items$form_ind)

  point = lp_obj$alias$point

  constraint = lp_obj$constraint


  if ("content" %in% names(constraint)) {

    content_cons <- lp_obj$constraint$content

    long_violation_by_cons <- data.frame(matrix(ncol = 5, nrow = 0,
                                  dimnames = list(NULL, c("cons_id", "form_ind", "n", "slice_unit", "vio"))))

    violation_by_cons = data.frame(cons_id = 1:nrow(content_cons), vio = 0)

    for (k in seq(nrow(content_cons))) {

      con = content_cons[k,]

      form_ind = con$FORM_IND[[1]]

      attribute = con$ATTRIBUTE

      if (attribute == "_OVERLAP" || attribute == "_ENEMY") next    # later

      if (con$SLICE_UNIT == "items") {

        if (attribute == "_FORM") {

          # for test length
          n = form_summary$n_item[which(form_summary$form_ind %in% form_ind)]

        } else

        if (attribute == "_CLUSTER") {

          # for test length and for total points
          cluster = which(content_cons$CLUSTER==as.numeric(con$ATTRIBUTE_VALUE))
          n = count_items_with_attr_from_forms (content_cons[cluster,], form_items[as.character(form_ind)])

        } else

        if (attribute == "_PASSAGE") { # items associated with passages

          if (!is.null(lp_obj$alias$psg_id))
             n <- form_items[form_ind] %>% map_int(function(x) length(which(!is.na(x[[lp_obj$alias$psg_id]])))) else
             n = rep(0, length(form_ind))

        } else

        # any constraint like _ENEMY_COL or _XXX_COL will be ignored for now
        if (substr(attribute, 1, 1) == "_") {} else
        {
          n = count_items_with_attr_from_forms (con, form_items[as.character(form_ind)])
        }

        if (con$SCOPE == "ACROSS") n = sum(n)
      }

      if (con$SLICE_UNIT == "points") {

        if (attribute == "_FORM") {

          # for total points in each form
          n = form_summary$n_point[which(form_summary$form_ind %in% form_ind)]

        } else

        if (attribute == "_CLUSTER") {

            # for test length and for total points

            cluster = which(content_cons$CLUSTER==as.numeric(con$ATTRIBUTE_VALUE))
            n = count_points_with_attr_from_forms (content_cons[cluster,], form_items[as.character(form_ind)], point)

        } else

        if (attribute == "_PASSAGE") { # points associated with passages

            if (!is.null(lp_obj$alias$psg_id))
              n <- form_items[form_ind] %>% map_int(function(x) {

                m = which(!is.na(x[[lp_obj$alias$psg_id]]))
                as.integer(sum(x[[lp_obj$alias$point]][m]))

              }) else
                n = rep(0, length(form_ind))

        } else

        # any constraint like _ENEMY_COL or _XXX_COL will be ignored for now
        if (substr(attribute, 1, 1) == "_") {} else

          n = count_points_with_attr_from_forms (con, form_items[as.character(form_ind)], point)

        if (con$SCOPE == "ACROSS") n = sum(n)

      }

      if (con$SLICE_UNIT == "passages") {

        if (attribute == "_PASSAGE") {

          if (!is.null(lp_obj$alias$psg_id))
            n <- form_items[form_ind] %>% map_int(function(x) {
              psg_id = x[[lp_obj$alias$psg_id]]
              length(unique(psg_id[!is.na(psg_id)]))
            }) else
              n = rep(0, length(form_ind))

        } else stop("Error")
      }


      temp = data.frame(cons_id = con$CONS_ID,
                         form_ind =
                          if (con$SCOPE == "WITHIN" || con$SCOPE == "" || is.na(con$SCOPE))
                            paste0(form_ind) else
                            paste0(form_ind, collapse=","),
                         n = n,
                         slice_unit = con$SLICE_UNIT,
                         vio = 0)

      if (any(n<con$SLICE_LOWER | n > con$SLICE_UPPER)) {
          vio_at = which(n<con$SLICE_LOWER | n > con$SLICE_UPPER)
          for (i in vio_at)
            if (n[i] < con$SLICE_LOWER) temp$vio[vio_at] = n[i] - con$SLICE_LOWER else
                                        temp$vio[vio_at] = n[i] - con$SLICE_UPPER
      }

      long_violation_by_cons = rbind(long_violation_by_cons, temp)
      violation_by_cons$vio[k] = sum(abs(temp$vio))

    }

    lp_obj$constraint_summary$content$long_violation_by_cons <- long_violation_by_cons
    lp_obj$constraint_summary$content$violation_by_cons <- violation_by_cons

  }

  # need to expand tif/tcc for panels?

  if (lp_obj$test_inf$n_panel > 1 &&
      length(lp_obj$constraint$tif$FORM_IND %>% unlist() %>% unique) * lp_obj$test_inf$n_panel == lp_obj$test_inf$n_form)
  {
    do_n_panel = lp_obj$test_inf$n_panel
    n_form_per_panel = as.integer(lp_obj$test_inf$n_form/lp_obj$test_inf$n_panel)
  } else {
    do_n_panel = 1
    n_form_per_panel = lp_obj$test_inf$n_form
  }

  # > $TIF.margin
  # test form theta TIF.obs TIF.lower TIF.upper
  # 1     1    1    -2  2.5707    2.4670    3.4670
  # 2     1    1    -1  5.3231    5.2490    6.2490

  if ("tif" %in% names(constraint)) {

    tif = lp_obj$constraint$tif

    if (!("SCOPE" %in% names(tif)))
      tif = bind_cols(tif, SCOPE = rep("WITHIN", nrow(tif)))

    ori_tif = tif

    tif_obs_tbl <- map(1:do_n_panel, ~{

      tif_obs = tibble(form_ind = list(integer()),
                       tif_id = character(),
                       theta = double(),
                       tif = double(),
                       tif_lower = double(),
                       tif_upper = double())
      tif = ori_tif
      which_panel = .x
      if (which_panel > 1)
        tif <- tif %>% mutate(FORM_IND = map(FORM_IND, ~.x+(which_panel-1)*n_form_per_panel))

      for (k in seq(nrow(tif))) {

        con = tif[k, ]

        forms = con$FORM_IND[[1]]

        theta = con$THETA

        column = intersect(names(form_items[[1]]), unlist(lp_obj$alias[grep("^irt_par", names(lp_obj$alias))]))

        if (con$SCOPE == "WITHIN" || con$SCOPE == "" || is.na(con$SCOPE)) {

          for (f in forms) {

            tif_val =
              round(sum(fisher(theta     = theta,
                               IRT.par   = form_items[[as.character(f)]][, column] ,
                               IRT.model = form_items[[as.character(f)]][[lp_obj$alias$irt_model]],
                               IRT.scale = form_items[[as.character(f)]][[lp_obj$alias$irt_scale]])), 4)

            tif_obs = dplyr::bind_rows(tif_obs,
                                       tibble(form_ind = list(f),
                                              tif_id = con$TIF_ID,
                                              theta = con$THETA,
                                              tif = tif_val,
                                              tif_lower = con$TIF_LOWER,
                                              tif_upper = con$TIF_UPPER))
          }
        }

        if (con$SCOPE == "ACROSS") {

          tif_val = unlist(lapply(forms, function (f) {
            round(sum(fisher(theta     = theta,
                             IRT.par   = form_items[[as.character(f)]][, column] ,
                             IRT.model = form_items[[as.character(f)]][[lp_obj$alias$irt_model]],
                             IRT.scale = form_items[[as.character(f)]][[lp_obj$alias$irt_scale]])), 4)}))

          tif_val = sum(tif_val)

          tif_obs = dplyr::bind_rows(tif_obs,
                                     tibble(form_ind = list(forms),
                                            tif_id = con$TIF_ID,
                                            theta = con$THETA,
                                            tif = tif_val,
                                            tif_lower = con$TIF_LOWER,
                                            tif_upper = con$TIF_UPPER))

        }
      }

      rownames(tif_obs) = NULL
      tif_obs
    }) %>% bind_rows()

    # if (!is.null(lp_obj$input_obj$module_structure)) {
    #
    #   tif_summary = lp_obj$input_obj$module_structure %>%
    #     select(form_ind = FORM_IND, module_name = MODULE_NAME) %>%
    #     unnest(form_ind) %>% right_join(tif_obs_tbl %>% unnest(form_ind)) %>%
    #     group_by(module_name, theta) %>%
    #     summarise(avg = round(mean(tif), 4), min = min(tif), max = max(tif), tif_target = tif_upper) %>% distinct()
    #
    #   lp_obj$constraint_summary$tif$tif_summary = tif_summary
    # }

  } else

    tif_obs_tbl = NULL

  lp_obj$constraint_summary$tif$tif_obj = tif_obs_tbl




  if ("tcc" %in% names(constraint)) {

    tcc = lp_obj$constraint$tcc

    if (!("SCOPE" %in% names(tcc)))
      tcc = bind_cols(tcc, SCOPE = rep("WITHIN", nrow(tcc)))

    ori_tcc = tcc

    tcc_obs_tbl <- map(1:do_n_panel, ~{

      tcc_obs = tibble(form_ind = list(integer()),
                       tcc_id = character(),
                       theta = double(),
                       tcc = double(),
                       tcc_lower = double(),
                       tcc_upper = double())
      tcc = ori_tcc
      which_panel = .x
      if (which_panel > 1)
        tcc <- tcc %>% mutate(FORM_IND = map(FORM_IND, ~.x+(which_panel-1)*n_form_per_panel))

      for (k in seq(nrow(tcc))) {

        con = tcc[k, ]

        forms = con$FORM_IND[[1]]

        theta = con$THETA

        column = intersect(names(form_items[[1]]), unlist(lp_obj$alias[grep("^irt_par", names(lp_obj$alias))]))

        if (con$SCOPE == "WITHIN" || con$SCOPE == "" || is.na(con$SCOPE)) {

          for (f in forms) {

            tcc_val = round(sum(expected_score(
              theta     = theta,
              IRT.par   = form_items[[as.character(f)]][, column] ,
              IRT.model = form_items[[as.character(f)]][[lp_obj$alias$irt_model]],
              IRT.scale = form_items[[as.character(f)]][[lp_obj$alias$irt_scale]])) /
              form_summary$n_point[which(form_summary$form_ind == f)], 4)

            tcc_obs = dplyr::bind_rows(tcc_obs,
                                       tibble(form_ind = list(f),
                                              tcc_id = con$TCC_ID,
                                              theta = con$THETA,
                                              tcc = tcc_val,
                                              tcc_lower = con$TCC_LOWER,
                                              tcc_upper = con$TCC_UPPER))
          }
        }

        if (con$SCOPE == "ACROSS") {

          tcc_val = unlist(lapply(forms, function (f) {
              sum(expected_score(theta     = theta,
                                       IRT.par   = form_items[[as.character(f)]][, column] ,
                                       IRT.model = form_items[[as.character(f)]][[lp_obj$alias$irt_model]],
                                       IRT.scale = form_items[[as.character(f)]][[lp_obj$alias$irt_scale]]))}))

          tcc_val = round(sum(tcc_val) / sum(form_summary$n_point[forms]), 4)

          tcc_obs = dplyr::bind_rows(tcc_obs,
                                     tibble(form_ind = list(forms),
                                            tcc_id = con$TCC_ID,
                                            theta = con$THETA,
                                            tcc = tcc_val,
                                            tcc_lower = con$TCC_LOWER,
                                            tcc_upper = con$TCC_UPPER))
        }
      }

      rownames(tcc_obs) = NULL
      tcc_obs
    }) %>% bind_rows()

  } else

    tcc_obs_tbl = NULL

  lp_obj$constraint_summary$tcc$tcc_obj = tcc_obs_tbl

  return(invisible(lp_obj))
}


