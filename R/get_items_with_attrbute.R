



get_items_with_attribute = function (con, items) {

  names(items) = toupper(names(items))
  con$ATTRIBUTE = toupper(con$ATTRIBUTE)
  attribute = con$ATTRIBUTE

  x = NULL
  # multiple attributes (must be categorical attributes)
  if (length(grep(":", attribute)) > 0) {

    attr = unlist(strsplit(attribute, ":"))
    val = unlist(strsplit(con$ATTRIBUTE_VALUE, ":"))

    m = matrix(c(attr, val), nrow = 2, byrow = T )
    colnames(m) <- paste0("pair_", 1:ncol(m))

    m = as_tibble(m)

    x = m %>% purrr::map(function(df) {

      # which(items[, df[1]] == df[2])
      values = unlist(strsplit(df[2], ",,"))
      which(sapply(items[, df[1]], function (i) i %in% values))

    })

    x = Reduce(intersect, x)

  } else {

    if (attribute %in% names(items)) {

      if (!is.na(con$ATTRIBUTE_VALUE) && con$ATTRIBUTE_VALUE == "NA") {

        x = which(is.na(items[[attribute]]))

      } else

      if (is.na(con$ATTRIBUTE_VALUE) || con$ATTRIBUTE_VALUE == "") {

        # use ATTRIBUTE_LOWER and ATTRIBUTE_UPPER
        x = which(items[[attribute]] >= con$ATTRIBUTE_LOWER & items[[attribute]] <= con$ATTRIBUTE_UPPER)

      } else {

        # ATTRIBUTE_VALUE

        values = unlist(strsplit(con$ATTRIBUTE_VALUE, ",,"))
        x = which(sapply(items[, attribute], function (i) i %in% values))

      }
    }
  }

  if (is.null(x)) {

    stop (paste0("get_items_with_attribute() cannot find any items for attribute--", attribute))

  }
  return (x)
}

count_items_with_attr_from_forms = function (cons, form_items) {

  # for each form
  n = sapply(1:length(form_items), function (f) {

    # get item ind for each constraint
    x = sapply(1:nrow(cons),
               function (j) get_items_with_attribute(con = cons[j,], form_items[[f]]))
    # union the x from all constraints in cons and get the number of those items
    length(Reduce(union, x))

  })

  return (n)
}


count_points_with_attr_from_forms = function (cons, form_items, point) {

  # for each form
  n = sapply(1:length(form_items), function (f) {

    # get item ind for each constraint
    x = sapply(1:nrow(cons),
               function (j) get_items_with_attribute(con = cons[j,], form_items[[f]]))

    # union the x from all constraints in cons and get the number of those items
    x = Reduce(union, x)

    # get the total for those items

    sum(form_items[[f]][, point][x])
  })

  return (n)
}



get_n_items_with_attribute = function (cons, items) {

  n = rep(0, nrow(cons))

  for (k in seq(nrow(cons))) {

    con = cons[k,]

    attribute = con$ATTRIBUTE

    # decide the items (x) involved with this attribute

    if (attribute == "_FORM")

      # for test length and for total points

      n[k] = nrow(items) else

        if (attribute == "_CLUSTER") {

          cluster = which(cons$CLUSTER==con$ATTRIBUTE_VALUE)
          x = sapply(cluster,
                     function (j) get_items_with_attribute(con = cons[j,], items))
          n[k] = length(Reduce(union, x))

        } else {

          x = get_items_with_attribute (con, items)
          # print(items$ENTITY_ID[x])
          n[k] = length(x)
        }
  }

  return (n)
}
