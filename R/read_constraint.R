
#' Read the constraints from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains testInf and xlsx file for constraints
#'
#' @param input_obj the constraint object
#'
#' @return a LP object
#'
#' @export
#'


read_constraint <- function (lp_obj, input_obj = NULL) {

  require(dplyr)
  if (!is.null(input_obj)) {

    # read in constraint information directly from an input_obj

    if (!is_tibble(input_obj)) input_obj = as_tibble(input_obj)
    consTbl = input_obj

  } else {

    file  = lp_obj$files$cons_file$file

    # nForm = lp_obj$test_inf$n_form

    if (!is.character(file)) {

      stop("No file is specified for the constraint")

    }


    if (is.xlsx.file(file)) {

      if (!file.exists(file))

        stop("Cannot find the file for the constraint")

      sheet = lp_obj$files$cons_file$sheet
      if (is.null(sheet)) sheet = 1

      # CONS_ID	FORM	CLUSTER	ATTRIBUTE	SCOPE	ATTRIBUTE_VALUE	ATTRIBUTE_LOWER
      # ATTRIBUTE_UPPER	SLICE_UNIT	SLICE_LOWER	SLICE_UPPER	MARGIN	NOTE

      # col_types = c("numeric", "text","text", "text", "text", "text",
      #               "numeric", "numeric", "text",
      #               "numeric", "numeric", "numeric", "text")
      col_types = c("numeric", "text","text", "text", "text", "text", "numeric",
                    "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric")

      consTbl = readxl::read_excel(file, sheet = sheet, col_types = NULL)

      # Commented out due to issue revealed by tibble 3, which rejects "text" as a column type.
      # https://www.tidyverse.org/blog/2020/04/tibble-3-0-0/
      # This was likely incorrect anyway, as "text" is the Excel column type, not the R data type.
      # readlx::read_excel returns Excel "text" columns as <chr> vectors.
      # Rollback tibble version with:
      #   devtools::install_version("tibble", version = "2.1.3", repos = "http://cran.us.r-project.org")
      # for(i in 1:ncol(consTbl))
      #   class(consTbl[[i]]) <- col_types[i]


    } else {

      col_types = c("icccccddciidc")  # add SOFT_LOWER and SOFT_UPPER later
      # c = character, i = integer, n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
      consTbl = read_gs (gs_file = file, sh = "constraint", col_types = col_types)[[1]]

    }
  }

  # FORM ==> FORM_IND

  # 1. any NA in FORM is converted to 1:n_form

  if (any(is.na(consTbl$FORM))) {

    # stop("Error--FORM cannot contain empty cells!")
    n_form = sum(lp_obj$test_inf$n_form)
    consTbl$FORM[which(is.na(consTbl$FORM))] = paste0("1:", n_form)
  }

  # 2. Add FORM_IND as a list of numeric column (e.g. "1:2" ==> c(1,2))

  # consTbl <- add_column(consTbl,
  #            FORM_IND = lapply(consTbl$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
  #            .before = 1)
  #
  # # 3. remove FORM from consTbl
  #
  # consTbl = dplyr::select(consTbl, -FORM)

  consTbl <- consTbl %>%
    add_column(
      FORM_IND = lapply(consTbl$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
      .before = 1) %>%
    dplyr::select(-FORM)

  # 4. replace NA with "WITHIN" for the column SCOPE

  consTbl$SCOPE[which(is.na(consTbl$SCOPE))] = "WITHIN"

  # add consTal to lp_obj as constraint

  lp_obj$constraint$content = consTbl

  # try to fix a silly typo in the SLICE_UNIN in spreadsheet

  lp_obj$constraint$content$SLICE_UNIT[grep("^i", lp_obj$constraint$content$SLICE_UNIT)] = "items"

  return(invisible(lp_obj))

}


dup_rows <- function (tdf, row_to_be_dup_at = nrow(tdf), n = 1, overwrite = NULL) {

  print(row_to_be_dup_at)

  dfBefore = if (row_to_be_dup_at > 1) tdf[1:(row_to_be_dup_at-1),] else NULL
  dupRow   = tdf[row_to_be_dup_at,]
  dfAfter  = if (row_to_be_dup_at < nrow(tdf)) tdf[(row_to_be_dup_at+1):nrow(tdf),] else NULL

  dup = replicate(n, dupRow, simplify = F)
  dup <- dplyr::bind_rows(dup)

  for (i in seq(length(overwrite))) {
    name = names(overwrite)[i]
    dup[[ name ]] = overwrite[[name]]
  }

  combinedDF <- dplyr::bind_rows(dfBefore, dup, dfAfter)

  print(nrow(combinedDF))
  return (combinedDF)

}


