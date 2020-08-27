
unify_labels = function (str.v, toUpper = T, toLower = F, connector = "\\.") {

  # trim leading or trailing spaces
  newStr = gsub("^\\s+|\\s+$", "", str.v)

  # using . to replace -, _, or a space
  # newStr = gsub("[-_ ]", "\\.", newStr)
  newStr = gsub("[-_\\. ]", connector, newStr)

  # remove multiple '.'
  newStr = gsub(paste0(connector, "+"), connector, newStr)

  # to upper cases
  if (toUpper) return(toupper(newStr))
  if (toLower) return(tolower(newStr))

  return(newStr)
}

data.frame2 <- function (...) {

    data.frame(..., stringsAsFactors = FALSE)
}

as.message <- function (x) {

    sink.file = file.path(tempdir(), "sink")

    sink(sink.file)

    print(x)

    sink()

    x = readLines(sink.file)

    message(paste(x, collapse = "\n"))

}

is.xlsx.file = function (file) {

  if (length(grep(".xlsx$", file, ignore.case = T)) > 0 || length(grep(".xls$", file, ignore.case = T)) > 0)
    return (T) else return (F)

}


bind_rows_2 = function (tbl1, tbl2) {

  by_names = intersect(names(tbl1), names(tbl2))

  list_col_at = which(tbl1 %>% summarise_all(class) == "list")

  if (length(list_col_at) > 0) {

    list_col = names(tbl1)[list_col_at]

    # by_names is used for full_join, which cannot contain list types
    by_names = setdiff(by_names, list_col)

    whole_df = full_join(tbl1, tbl2, by = by_names)

    # combine list columns
    to_combine_list = intersect(list_col, names(tbl2))

    for (k in 1:seq_along(to_combine_list)) {
      to_combine_list.x.y = paste0(to_combine_list[k], c(".x", ".y"))
      x_y_at = which(names(whole_df) %in% to_combine_list.x.y)

      combined_col = sapply(1:nrow(whole_df),
                            function (i) c(whole_df[, x_y_at[1]][[1]][[i]], whole_df[, x_y_at[2]][[1]][[i]]))
      whole_df <- whole_df %>% select(-x_y_at) %>% mutate(new_col = combined_col)

      names(whole_df)[ncol(whole_df)] = to_combine_list[k]
    }

  } else

    whole_df = full_join(tbl1, tbl2, by = by_names)

  return (whole_df)
}


#' change column data types
#'
#' @param tbl1 a regular data frame or a tibble
#'
#' @param col_types the column types include 'c' for character, 'd' for double,
#' 'i' for integer, 'l' for logical, and 'f' for factor.
#'
#' @return a tibble or a data frame with changed column types
#'
#' @export
#'

mutate_col_type = function (tbl1, col_types) {

  types = sapply(seq(nchar(col_types)),
                 function(i) substr(x = col_types, start = i, stop = i))

  tbl2 <- tbl1 %>% as_tibble() %>%
    mutate_if(types == "c", as.character) %>%
    mutate_if(types == "d", as.double) %>%
    mutate_if(types == "i", as.integer) %>%
    mutate_if(types == "l", as.logical) %>%
    mutate_if(types == "f", as.factor)

  return (tbl2)
}

#' A merge function
#'
#' 1. An NA value in the first data frame matches to everything in the second data frame;
#' 2. The ordering of both rows and columns in the first data frame is preserved.
#'
#' @param x the first data frame
#'
#' @param y the second data frame
#'
#' @return
#' A data frame with the same columns as x
#'
#' @export


merge2 = function (x, y, all.x = FALSE) {

    b = intersect(names(x), names(y))
    z = list()

    for (i in seq(nrow(x))) {
      xi = x[i,]
      w = which(is.na(xi[b]))
      if (length(w) > 0) {
        xi[b[w]] = NULL
        bi = b[-w]
      }
      else
        bi = b

      z[[i]] = merge(xi, y, by = bi, all.x = all.x)
    }

    z = do.call(rbind, z)

    # order the columns

    columns = union(names(x), names(y))

    z = z[columns]

    return(z)
  }

#' A duplicated function that identifies if an element is unique or not
#'
#' @param vec a vector
#'
#' @return
#' A vector of F/T
#'
#' @export
duplicated2 = function (vec) {

  return (duplicated(vec, fromLast = TRUE) | duplicated(vec, fromLast = FALSE))

}


