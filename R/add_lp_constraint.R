
#' create LP constraints
#'
#' @description the objectives include "PASSAGE_SLICE", "FILTER", "CONTENT", "TIF", "TCC",
#' "MIN_OVERLAP", "ITEM_USAGE"
#'
#' @param lp_obj the lp object contains test_inf and xlsx file for constraints
#'
#' @return a LP object
#'
#' @export
#'
#'





add_lp_constraint = function (lp_obj) {

  require(dplyr)
  require(stringr)

  if (!("items" %in% names(lp_obj)))
  {
    stop("The lp object does not have an item data frame")
  }

  if (!("constraint" %in% names(lp_obj)))
  {
    stop("The lp object does not have a constraint data frame")
  }

  items = lp_obj$items
  form_inf = lp_obj$form_inf
  alias = lp_obj$alias

  n.items = nrow(items)
  n.forms = lp_obj$test_inf$n_form

  constraint = lp_obj$constraint
  objective.methods = toupper(lp_obj$objective)   # i.e. spec

  n_overlap_forms = 0

  # get number of overlap forms from the content table

  if (length(which(constraint$content$ATTRIBUTE == "_OVERLAP" &
    constraint$content$SLICE_UPPER > 0)) > 0)
       n_overlap_forms <- nrow(constraint$content[which(constraint$content$ATTRIBUTE == "_OVERLAP" &
                                                        constraint$content$SLICE_UPPER > 0), ])

  # Add x variables, either alone or with ys variables for PASSAGES
  # need to consider "PASSAGE_SLICE" is not used by _PASSAGE is in content constraint later
  if ("PASSAGE_SLICE" %in% objective.methods && !is.null(lp_obj$passage_index)) {
    n.psgs = nrow(lp_obj$passage_index)
    n.x = n.forms * (n.items + n.psgs)
    ys.start.at = n.forms * n.items + 1
  } else {
    n.psgs = 0
    n.x = (n.forms + n_overlap_forms) * n.items
    ys.start.at = 0
  }
  objective = rep(0, n.x)
  var.type = rep("binary", n.x)
  lower = rep(0, n.x)
  upper = rep(1, n.x)

  warning(str_c("n_overlap_forms=", n_overlap_forms))

  # Add y variables for MIN_OVERLAP
  if ( ("MIN_OVERLAP" %in% objective.methods) & (n.forms > 1)) {
    n.y = n.items * (n.forms - 1)
    y.index = n.x + 1

    objective = c(objective, rep(seq(n.forms - 1), n.items))
    var.type = c(var.type, rep("binary", n.y))
    lower = c(lower, rep(0, n.y))
    upper = c(upper, rep(1, n.y))
  } else {
    n.y = 0
  }

  # Add z variables for SOFT content constraints
  if ("CONTENT" %in% objective.methods && "content" %in% names(constraint) && "MARGIN_WEIGHT" %in% names(constraint$content)) {
    cons.weight = constraint$content$MARGIN_WEIGHT[!is.na(constraint$content$MARGIN_WEIGHT)]

    n.z = 2 * length(cons.weight)
    z.index = n.x + n.y + 1

    objective = c(objective, rep(cons.weight, each = 2))
    var.type = c(var.type, rep("integer", n.z))
    lower = c(lower, rep(0, n.z))
    upper = c(upper, rep(NA, n.z)) # No upper bound, should be bound.type GLP_LO in set_glpk.R
  } else {
    n.z = 0
  }

  # Add z.tif variables for SOFT tif constraints
  if ("TIF" %in% objective.methods && "tif" %in% names(constraint) && "MARGIN_WEIGHT" %in% names(constraint$tif)) {
    cons.weight = constraint$tif$MARGIN_WEIGHT[!is.na(constraint$tif$MARGIN_WEIGHT)]

    n.z.tif = 2 * length(cons.weight)
    z.tif.index = n.x + n.y + n.z + 1

    objective = c(objective, rep(cons.weight, each = 2)) # tif penalty coefficients can be any value, does not have to always be 1
    var.type = c(var.type, rep("real", n.z.tif)) # floating point type, because tif constraints use inf as coefficients
    lower = c(lower, rep(0, n.z.tif))
    upper = c(upper, rep(NA, n.z.tif)) # No upper bound, should be bound.type GLP_LO in set_glpk.R
  } else {
    n.z.tif = 0
  }

  # Start creating lp_cons

  # 0. initialize lp_cons
  lp_cons = NULL      # i.e. constraint

  # 1. add filter constraints
  if ( "FILTER" %in% objective.methods && !is.null(lp_obj$constraintfilter))
    lp_cons = gen_filter_constraint(lp_obj$constraint$filter, label = "filter")

  # 2. add content constraints including:
  #    blueprint
  #    _FORM for module size and test length
  #    _CLUSTER for combined blueprint items
  #    _OVERLAP for controlling overlap for forms/modules

  if ( "CONTENT" %in% objective.methods && "content" %in% names(constraint) ) {

    if (n_overlap_forms > 0) {
      auxiliary_groups = (n.forms+1):(n.forms+n_overlap_forms)
      next_auxiliary_group = 1
    }
    cons = constraint$content

    for (k in seq(nrow(cons))) {

      con = cons[k,]

      attribute = con$ATTRIBUTE
      lab <- if (is.na(con$NOTE)) paste0("bp-", con$CONS_ID, " ", attribute) else
                                  paste0("bp-", con$CONS_ID, " ", attribute, " ", con$NOTE)
      forms = con$FORM_IND[[1]]
      n.forms.0 = length(forms)

      # decide the items (x) involved with this attribute

      # Skip _ENEMY and _XXX_COL (_ENEMY_COL).  These will be handled later.
      xxx_regex = "^_([^_]+)_COL(_W_NA)?$"
      if (attribute == "_ENEMY" || grepl(xxx_regex, attribute)) next

      if (attribute == "_FORM" || attribute == "_OVERLAP") {

        # for test length and for total points

        x = 1:n.items

      } else

      if (attribute == "_PASSAGE") {

        if (con$SLICE_UNIT == "points" || con$SLICE_UNIT == "items")
          x = 1:n.items

        if (con$SLICE_UNIT == "passages") {

          ys = (n.forms.0 * n.items + 1):(n.forms.0 * n.items + n.psgs)
        }

      } else

      if (attribute == "_CLUSTER") {

          cluster = which(cons$CLUSTER==as.numeric(con$ATTRIBUTE_VALUE))
          x = sapply(cluster,
                     function (j) get_items_with_attribute(con = cons[j,], items))
          x = Reduce(union, x)

      } else

          x = get_items_with_attribute (con, items)

      # checking x

      if (length(x) == 0) {

        if (con$SLICE_LOWER <= 0) next else {

          message("No items are available for the following constraint:\n")

          as.message(con)

          if (lp_obj$options$timeout > 0)
            stop(paste0("No items are available for constraint ", k, " with attribute ", con$ATTRIBUTE))
        }
      }

      if (attribute == "_OVERLAP") {

        k = length(lp_cons)

        if (con$SLICE_LOWER == 0 && con$SLICE_UPPER == 0) {

          # no overlap across those forms

          for (i in seq(n.items))
          {
              lp_cons[[k + i]] = gen_constraint_across_forms(
                forms,
                n.items,
                x = i,
                x.coef = 1,
                type = "<=",
                rhs = 1,
                label = "NO Overlap",
                consId = con$CONS_ID)[[1]]
          }

        } else {

          # contraint of the number of overlap items

          auxiliary_group = auxiliary_groups[next_auxiliary_group]
          next_auxiliary_group = next_auxiliary_group + 1

          # m2_3 has ? items
          lp_cons[[length(lp_cons) + 1]] =
            gen_constraint_within_forms(auxiliary_group, n.items, x = x, x.coef = rep(1, length(x)),
                                        type = "<=", rhs = con$SLICE_UPPER, label = "Overlap", consId = con$CONS_ID)[[1]]


          lp_cons[[length(lp_cons) + 1]] =
            gen_constraint_within_forms(auxiliary_group, n.items, x = x, x.coef = rep(1, length(x)),
                                        type = ">=", rhs = con$SLICE_LOWER, label = "Overlap", consId = con$CONS_ID)[[1]]

          # make sure m3 has the same 4 items in the combined section 10
          k = length(lp_cons)

          for (i in seq(n.items))
          {
            lp_cons[[k + i]] = gen_constraint_across_forms(
              c(con$FORM_IND[[1]], auxiliary_group),
              n.items,
              x = i,
              x.coef = c(1,1,-2),
              type = "<=",
              rhs = 1,
              label = "NO Overlap",
              consId = con$CONS_ID)[[1]]
          }

          k = length(lp_cons)
          for (i in seq(n.items))
          {
            lp_cons[[k + i]] = gen_constraint_across_forms(
              c(con$FORM_IND[[1]], auxiliary_group),
              n.items,
              x = i,
              x.coef = c(1,1,-2),
              type = ">=",
              rhs = 0,
              label = "NO Overlap",
              consId = con$CONS_ID)[[1]]
          }
        }

      } else { # other constraints

        if (con$SLICE_UNIT == "points") {

          x.coef = unname(unlist((lp_obj$items[x, alias$point])))

        }

        if (con$SLICE_UNIT == "items") {

          x.coef = rep(1, length(x))
        }

        if (con$SLICE_UNIT == "passages")  {

          ys.coef = rep(1,length(ys))

          constraint.fun = switch(con$SCOPE,
                                  WITHIN = "gen_psgcount_constraint_within_forms",
                                  ACROSS = "gen_psgcount_constraint_across_forms")

          con.lower = do.call(constraint.fun,
                              list(
                                forms,
                                n.psgs,
                                ys.start.at,
                                ys,
                                ys.coef,
                                type = ">=",
                                rhs = con$SLICE_LOWER,
                                label = lab,
                                consId = con$CONS_ID))
          con.upper = do.call(constraint.fun,
                              list(
                                forms,
                                n.psgs,
                                ys.start.at,
                                ys,
                                ys.coef,
                                type = "<=",
                                rhs = con$SLICE_UPPER,
                                label = lab,
                                consId = con$CONS_ID))

          lp_cons = c(lp_cons, con.lower, con.upper)

        } else {

          if ((!"MARGIN_WEIGHT" %in% names(con)) || is.na(con$MARGIN_WEIGHT)) {

            # Generate hard constraint
            constraint.fun = switch(con$SCOPE,
                                    WITHIN = "gen_constraint_within_forms",
                                    ACROSS = "gen_constraint_across_forms")

            con.lower = do.call(constraint.fun,
                                list(
                                  forms,
                                  n.items,
                                  x,
                                  x.coef,
                                  type = ">=",
                                  rhs = con$SLICE_LOWER,
                                  label = lab,
                                  consId = con$CONS_ID)) # "blueprint"))

            con.upper = do.call(constraint.fun,
                                list(
                                  forms,
                                  n.items,
                                  x,
                                  x.coef,
                                  type = "<=",
                                  rhs = con$SLICE_UPPER,
                                  label = lab,
                                  consId = con$CONS_ID)) # "blueprint"))

            lp_cons = c(lp_cons, con.lower, con.upper)

          } else {

            # Generate soft constraint
            soft.con.lab = paste0("soft-", lab)
            soft.con.index = c(z.index, z.index+1)
            z.index = z.index + 2

            con.lower = do.call(constraint.fun,
                                list(
                                  forms,
                                  n.items,
                                  x,
                                  x.coef,
                                  y = soft.con.index,
                                  y.coef = c(1,0),
                                  type = ">=",
                                  rhs = con$SLICE_LOWER,
                                  label = soft.con.lab)) # "soft" lower

            con.upper = do.call(constraint.fun,
                                list(
                                  forms,
                                  n.items,
                                  x,
                                  x.coef,
                                  y = soft.con.index,
                                  y.coef = c(0,-1),
                                  type = "<=",
                                  rhs = con$SLICE_UPPER,
                                  label = soft.con.lab)) # "soft" upper

            lp_cons = c(lp_cons, con.lower, con.upper)

          }

        }
      }

    }
  }

  # 3. add statistic constraints

  if (!is.null(lp_obj$constraint$statistic)) {

  }

  # 4. add enemy constraints
  # 4.a constraint$enemy is provided through an enemy.xlsx file and content had "_ENEMY" attribute

  if (!is.null(lp_obj$constraint$enemy)) {
    enemy_cons = lp_obj$constraint$content %>%
      filter(ATTRIBUTE == "_ENEMY") %>%
      select(FORM_IND, CONS_ID, SCOPE, ATTRIBUTE_VALUE) %>%
      rename(ENEMY_GROUP_ID = ATTRIBUTE_VALUE) %>%
      left_join(lp_obj$constraint$enemy, by = "ENEMY_GROUP_ID")

    for (i in seq(nrow(enemy_cons))) {
      # i = 1
      if (enemy_cons$SCOPE[[i]] == "ACROSS") {
        enemy_lp_cons = gen_constraint_across_forms(
          enemy_cons$FORM_IND[[i]],
          n.items,
          x = enemy_cons$ITEM_IND[[i]],
          x.coef = rep.int(1, length(enemy_cons$ITEM_IND[[i]])),
          type = "<=",
          rhs = 1,
          label = paste0("enemy-", enemy_cons$ENEMY_GROUP_ID[[i]]),
          consId = enemy_cons$CONS_ID
        )
      } else {
        enemy_lp_cons = gen_constraint_within_forms(
          enemy_cons$FORM_IND[[i]],
          n.items,
          x = enemy_cons$ITEM_IND[[i]],
          x.coef = rep.int(1, length(enemy_cons$ITEM_IND[[i]])),
          type = "<=",
          rhs = 1,
          label = paste0("enemy-", enemy_cons$ENEMY_GROUP_ID[[i]]),
          consId = enemy_cons$CONS_ID
        )
      }

      lp_cons = c(lp_cons, enemy_lp_cons)
    }
  }

  # 4.b handle any _XXX_COL and _XXX_COL_W_NA patterns
  # example: content had "_ABC_COL" attribute, which means items has "ABC" column
  #   FORM_IND  CONS_ID SCOPE  GROUP_ID      ITEM_ID ITEM_IND
  #   <list>      <chr> <chr>  <chr>          <list>    <list>
  # 1 <dbl [3]>     243 ACROSS eg-01          <chr [1]> <int [1]>
  # 2 <dbl [3]>     244 ACROSS eg-02          <chr [4]> <int [4]>
  # 3 <dbl [3]>     245 ACROSS eg-03          <chr [3]> <int [3]>
  xxx_attrs = sort(unique(grep(xxx_regex, lp_obj$constraint$content$ATTRIBUTE, value = T)))
  # Skip any _XXX_COL where column items$XXX does not exist.  example: _ABC_COL but no items$ABC
  xxx_attrs = xxx_attrs[gsub(xxx_regex, "\\1", xxx_attrs) %in% names(items)]

  for (xxx_attr in xxx_attrs) {
    # xxx_attr = xxx_attrs[[1]]
    xxx_col = gsub(xxx_regex, "\\1", xxx_attr)
    xxx_with_na = grepl("_W_NA$", xxx_attr)

    # _XXX_COL_WITH_NA is not fully implemented yet!
    # See the line below: if (any(is.na(xxx_cons$GROUP_ID)))
    # In this if block, need to figure out how to create the
    # xxx_cons groups when NA should be treated as its own group
    if (any(xxx_with_na)) {
      stop("_XXX_COL_W_NA is not fully implemented yet!  See myFormAssembler::add_lp_constraint")
    }

    xxx_groups = items %>% pull(xxx_col) %>% unique() %>% as.character()
    if (!xxx_with_na) {
      xxx_groups = setdiff(xxx_groups, NA_character_)
    }

    xxx_cons = lp_obj$constraint$content %>%
      filter(ATTRIBUTE == xxx_attr) %>%
      select(FORM_IND, CONS_ID, SCOPE, ATTRIBUTE_VALUE) %>%
      rename(GROUP_ID = ATTRIBUTE_VALUE)

    if (any(is.na(xxx_cons$GROUP_ID))) {
      xxx_cons = map_df(1:nrow(xxx_cons), ~{
        cons = xxx_cons[.x,]
        if (!is.na(cons$GROUP_ID[1])) cons else {
          tibble (FORM_IND = cons$FORM_IND,
                  CONS_ID = str_c(cons$CONS_ID, "_", xxx_col, "_", xxx_groups),
                  SCOPE = cons$SCOPE,
                  GROUP_ID = xxx_groups)
        }
      })
    }

    xxx_group_items = items %>% mutate(ITEM_IND = 1:nrow(.)) %>%
      select(alias$item_id, ITEM_IND, xxx_col)
    if (!xxx_with_na) {
      xxx_group_items = xxx_group_items %>% drop_na()
    }
    xxx_group_items = xxx_group_items %>% group_by(!!sym(xxx_col))

    xxx_group_items = xxx_group_items %>% group_map(
        ~tibble(ITEM_ID = list(.x[[alias$item_id]]),
                ITEM_IND = list(.x$ITEM_IND)), .keep = T) %>%
      set_names(unlist(group_keys(xxx_group_items))) %>%
      bind_rows(.id = xxx_col) %>% set_names(c("GROUP_ID", alias$item_id, "ITEM_IND"))

    xxx_cons = xxx_cons %>% left_join(xxx_group_items, by = "GROUP_ID")

    for (i in seq(nrow(xxx_cons))) {
      # i = 7000
      if (xxx_cons$SCOPE[[i]] == "ACROSS") {
        xxx_lp_cons = gen_constraint_across_forms(
          xxx_cons$FORM_IND[[i]],
          n.items,
          x = xxx_cons$ITEM_IND[[i]],
          x.coef = rep.int(1, length(xxx_cons$ITEM_IND[[i]])),
          type = "<=",
          rhs = 1,
          label = paste0(tolower(xxx_col), "-", xxx_cons$GROUP_ID[i]),
          consId = xxx_cons$CONS_ID[i]
        )
      } else {
        xxx_lp_cons = gen_constraint_within_forms(
          xxx_cons$FORM_IND[[i]],
          n.items,
          x = xxx_cons$ITEM_IND[[i]],
          x.coef = rep.int(1, length(xxx_cons$ITEM_IND[[i]])),
          type = "<=",
          rhs = 1,
          label = paste0(tolower(xxx_col), "-", xxx_cons$GROUP_ID[[i]]),
          consId = xxx_cons$CONS_ID[i]
        )
      }

      lp_cons = c(lp_cons, xxx_lp_cons)
    }
  }

  # 5. add objective method--"TIF"

  # if ("TIF" %in% objective.methods)
  if ("TIF" %in% objective.methods && "tif" %in% names(constraint)) {

      tif = lp_obj$constraint$tif

      if (!("SCOPE" %in% names(tif)))
        tif = bind_cols(tif, SCOPE = rep("WITHIN", nrow(tif)))

      x = seq(n.items)

      for (k in seq(nrow(tif))) {

        con = tif[k, ]

        forms = con$FORM_IND[[1]]

        theta = con$THETA

        # column = get_pool_IRT_PAR_col (alias, names(lp_obj$items))
        column = intersect(names(items), unlist(alias[grep("^irt_par", names(alias))]))

        x.coef = fisher(theta     = theta,
                        IRT.par   = as.data.frame(items[, column]),
                        IRT.model = items[[alias$irt_model]],
                        IRT.scale = items[[alias$irt_scale]])

        constraint.fun = switch(con$SCOPE,
                                WITHIN = "gen_constraint_within_forms",
                                ACROSS = "gen_constraint_across_forms")

        rhs.lower = con$TIF_LOWER
        rhs.upper = con$TIF_UPPER

        if ((!"MARGIN_WEIGHT" %in% names(con)) || is.na(con$MARGIN_WEIGHT)) {

          # Generate hard constraint
          con.lower = do.call(constraint.fun, list(
                                forms,
                                n.items,
                                x,
                                x.coef,
                                type = ">=",
                                rhs = rhs.lower,
                                label = paste0("tif-", con$TIF_ID, "-hard-lower")
                              ))

          con.upper = do.call(constraint.fun, list(
                                forms,
                                n.items,
                                x,
                                x.coef,
                                type = "<=",
                                rhs = rhs.upper,
                                label = paste0("tif-", con$TIF_ID, "-hard-upper")
                              ))

        } else {

          # Generate soft constraint
          soft.con.index = c(z.tif.index, z.tif.index+1)
          z.tif.index = z.tif.index + 2

          con.lower = do.call(constraint.fun, list(
                                forms,
                                n.items,
                                x,
                                x.coef,
                                y = soft.con.index,
                                y.coef = c(1,0),  # tif.lower soft coef 1 gives direction: +penalty in exchange for +inf to reach tif.lower
                                type = ">=",
                                rhs = rhs.lower,
                                label = paste0("tif-", con$TIF_ID, "-soft-lower")
                              ))

          con.upper = do.call(constraint.fun, list(
                                forms,
                                n.items,
                                x,
                                x.coef,
                                y = soft.con.index,
                                y.coef = c(0,-1),  # tif.upper soft coef -1 gives direction: +penalty in exchange for -inf to reach tif.upper
                                type = "<=",
                                rhs = rhs.upper,
                                label = paste0("tif-", con$TIF_ID, "-soft-upper")
                              ))

        }

        lp_cons = c(lp_cons, con.lower, con.upper)

    }
  }

  # 8. add objective method--"TCC"

  # if ("TCC" %in% objective.methods) {
  if ("TCC" %in% objective.methods && "tcc" %in% names(constraint)) {

    tcc = lp_obj$constraint$tcc

    if (!("SCOPE" %in% names(tcc)))
      tcc = bind_cols(tcc, SCOPE = rep("WITHIN", nrow(tcc)))

    x = seq(n.items)


    for (k in seq(nrow(tcc))) {

      con = tcc[k, ]

      forms = con$FORM_IND[[1]]

      theta = con$THETA

      column = intersect(names(items), unlist(alias[grep("^irt_par", names(alias))]))

      x.coef = expected_score (theta     = theta,
                               IRT.par   = as.data.frame(items[, column]),
                               IRT.model = items[[alias$irt_model]],
                               IRT.scale = items[[alias$irt_scale]])

      # re-write for WITHIN and ACROSS

      # if (length(unique(form_inf$n_point_l[forms])) > 1)
      #   stop (paste0("For TCC constraint, there are different total points for forms ", paste0(forms, collapse = ",")))
      #
      # rhs.lower = con$TCC_LOWER * form_inf$n_point_l[forms[1]]
      # rhs.upper = con$TCC_UPPER * form_inf$n_point_u[forms[1]]
      #
      # if (!is.na(rhs.lower)) {
      #   con.lower = gen_constraint_within_forms(
      #     forms,
      #     n.items,
      #     x,
      #     x.coef,
      #     type = ">=",
      #     rhs = rhs.lower,
      #     label = paste0("tcc_", con$TCC_ID, "_lower"))
      #   lp_cons = c(lp_cons, con.lower)
      # }
      #
      # if (!is.na(rhs.upper)) {
      #   con.upper = gen_constraint_within_forms(
      #     forms,
      #     n.items,
      #     x,
      #     x.coef,
      #     type = "<=",
      #     rhs = rhs.upper,
      #     label = paste0("tcc_", con$TCC_ID, "_upper"))
      #   lp_cons = c(lp_cons, con.upper)
      # }

      constraint.fun = switch(con$SCOPE,
                              WITHIN = "gen_constraint_within_forms",
                              ACROSS = "gen_constraint_across_forms")

      rhs.lower = switch(con$SCOPE,
                              WITHIN = con$TCC_LOWER * form_inf$n_point_l[forms],
                              ACROSS = con$TCC_LOWER * sum(form_inf$n_point_l[forms]))

      rhs.upper = switch(con$SCOPE,
                         WITHIN = con$TCC_UPPER * form_inf$n_point_u[forms],
                         ACROSS = con$TCC_UPPER * sum(form_inf$n_point_u[forms]))

      con.lower = do.call(constraint.fun,
                          list(
                            forms,
                            n.items,
                            x,
                            x.coef,
                            type = ">=",
                            rhs = rhs.lower,
                            label = "tcc"))

      con.upper = do.call(constraint.fun,
                          list(
                            forms,
                            n.items,
                            x,
                            x.coef,
                            type = "<=",
                            rhs = rhs.upper,
                            label = "tcc"))

      lp_cons = c(lp_cons, con.lower, con.upper)

    }
  }


  # 5. add objective method--"MIN_DIFF_DIST"

  if ("MIN_DIFF_DIST" %in% objective.methods) {


  }

  # 6. add objective method--"MIN_OVERLAP"

  if ( ("MIN_OVERLAP" %in% objective.methods) & (n.forms > 1)) {

    k = length(lp_cons)

    for (i in seq(n.items))
    {
      for (m in seq(n.forms - 1))
      {
        k = k + 1

        lp_cons[[k]] = gen_constraint_across_forms(
          seq(n.forms),
          n.items,
          x = i,
          x.coef = 1,
          y = y.index,
          y.coef = -n.forms,
          type = "<=",
          rhs = m,
          label = "min_overlap")[[1]]

        y.index = y.index + 1
      }
    }
  }

  # 7. add objective method--"ITEM_USAGE"

  if ("ITEM_USAGE" %in% objective.methods) {

    # do this later because the n.x need to be re-do
    # add usage when read in items; do this later

    if ("usage" %in% (names(items))) max.usage = items$usage else
      max.usage = rep(1, nrow(items))

    k = length(lp_cons)

    for (i in seq(n.items))
    {
      lp_cons[[k + i]] = gen_constraint_across_forms(
        seq(n.forms),
        n.items,
        x = i,
        x.coef = 1,
        type = "<=",
        rhs = max.usage[i],
        label = "item_usage")[[1]]
    }
  }


  # 8. add objective method--"PASSAGE_SLICE"

  if ("PASSAGE_SLICE" %in% objective.methods) {

    passage_slice = lp_obj$constraint$passage_slice

    # FORM_IND  PSG_ID UNIT  LOWER UPPER MARGIN NOTE
    # <list>    <chr>  <chr> <dbl> <dbl>  <dbl> <chr>
    # 1 <int [1]> psg01  items    6.    6.     0. NA
    # 2 <int [1]> psg02  items    6.    6.     0. NA

    passage_index = lp_obj$passage_index

    psg.ids = items[[alias$psg_id]]

    for (k in seq(nrow(passage_slice)))
    {
      con = passage_slice[k, ]

      x = which(psg.ids == con$PSG_ID) # the item ind assocaited with the k-th passage

      rhs.lower = max(con$LOWER- con$MARGIN, 0)
      rhs.upper = con$UPPER + con$MARGIN

      if (length(x) == 0)
      {
        if (rhs.lower <= 0)
        {
          next
        }
        else
        {
          message("No items are available for this passage:\n")

          as.message(con)

          if (parcc$options$timeout > 0)
            stop("The PASSAGE_SLICE objective cannot be satisfied")
        }
      }

      x.coef = switch(as.character(con$UNIT),
                      points = items[[alias$points]][x],
                      items  = rep(1, length(x)))

      forms = unlist(con$FORM_IND)

      # n.forms = length(forms)

      # x1+x2+x3+x4+x5+x6 - 3y1 <=0
      # x1+x2+x3+x4+x5+x6 - 3y1 >=0

      psg.ind = pmatch(con$PSG_ID, passage_index$psg.id)
      ys = n.x - (n.psgs * n.forms.0) + psg.ind

      # con.lower = gen_constraint_within_forms(
      # forms, n.items, n.psg, x, x.coef,
      # type, rhs, label = "", ys = NULL, ys.coef = NULL
      con.lower = gen_group_constraint_within_forms(
        forms,  # only one form at a time
        n.items,
        n.psgs,
        x,
        x.coef,
        type = ">=",
        rhs = 0,
        label = "passage.slice",
        ys = ys,
        ys.coef = -rhs.lower)

      # con.upper = gen_constraint_within_forms(
      con.upper = gen_group_constraint_within_forms(
        forms,  # only one form at a time
        n.items,
        n.psgs,
        x,
        x.coef,
        type = "<=",
        rhs = 0,
        label = "passage.slice",
        ys,
        ys.coef = -rhs.upper)

      lp_cons = c(lp_cons, con.lower, con.upper)
    }

  }


  # 9. add objective method--"MIN_ITEM_USAGE_WT"

  if ("MIN_ITEM_USAGE_WT" %in% objective.methods) {

    # need to consider "PASSAGE_SLICE" is not used by _PASSAGE is in content constraint later
    usingPassageSlice = "PASSAGE_SLICE" %in% objective.methods && !is.null(lp_obj$passage_index)
    formSeq = seq_len(ifelse(usingPassageSlice, n.forms, n.forms + n_overlap_forms))

    for (k in formSeq) {
      # k = 1
      x.form.len = ifelse(usingPassageSlice, n.items + n.psgs, n.items)
      x.form.start = ((k - 1) * x.form.len) + 1
      x.ind = seq.int(from = x.form.start, length.out = length(lp_obj$items$OBJ_WEIGHT))

      # overwrite the x objective coefficients with the values for the "min_item_usage_wt" objective
      objective[x.ind] = lp_obj$items$OBJ_WEIGHT
    }

  }


  # put everything into the lp object
  lp = list(
    objective = objective,
    constraint = lp_cons,
    var.type = var.type,
    lower = lower,
    upper = upper)

  # update the lp_obj object

  lp_obj$lp = lp
  lp_obj$form_inf = form_inf

  # remove components derived from the previous lp object
  # to avoid inconsistency

  lp_obj$output = NULL


  return(invisible(lp_obj))

}


