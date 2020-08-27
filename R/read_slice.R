
#' Read the slice from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains testInf and xlsx file for slice
#'
#' @param input_obj the slice object
#'
#' @return a LP object
#'
#' @export
#'


read_slice <- function (lp_obj, input_obj = NULL, verbose = F) {

  if (verbose) print ("call read_slice()")

  if (!is.null(input_obj)) {

    # read in pool information directly from a input_obj

    if (verbose)
      print(input_obj)

    sliceTbl = mutate_col_type (input_obj, col_types = "cccccddciicc")

  } else {

    file  = lp_obj$files$slice_file$file

    sheet = lp_obj$files$slice_file$sheet
    if (is.null(sheet)) sheet = 1

    # nForm = lp_obj$test_inf$n_form

    if (!is.character(file)) {

      stop("No file is specified for the slice")

    }

    if (is.xlsx.file(file)) {

      if (!file.exists(file)) {

        stop("Cannot find the file for the slice")
      }

      col_types = c("text","text", "text", "text", "text",
                    "numeric", "numeric", "text",
                    "numeric", "numeric", "numeric", "text")

      # FORM	CLUSTER	ATTRIBUTE	SCOPE	ATTRIBUTE_VALUE	ATTRIBUTE_LOWER	ATTRIBUTE_UPPER	SLICE_UNIT	SLICE_LOWER	SLICE_UPPER	MARGIN	NOTE
      sliceTbl = readxl::read_excel(file,
                                   col_types = col_types)
    } else {

      # c = character, i = integer, n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
      col_types = c("cccccddciicc")
      sliceTbl = read_gs (gs_file = file, sh = "alias", col_types = col_types)[[1]]

    }
  }

  # FORM ==> FORM_IND

  # 1. FORM cannot contain NA

  if (any(is.na(sliceTbl$FORM))) {

    stop("Error--FORM cannot contain empty cells!")

  }

  # 2. Add FORM_IND as a list of numeric column (e.g. "1:2" ==> c(1,2))

  sliceTbl <- sliceTbl %>%
    add_column(
      FORM_IND = lapply(sliceTbl$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
      .before = 1) %>%
    dplyr::select(-FORM)

  # 3. replace NA with "WITHIN" for the column SCOPE

  sliceTbl$SCOPE[which(is.na(sliceTbl$SCOPE))] = "WITHIN"

  # add sliceTal to lp_obj as content constraints

  # lp_obj$constraint$content <- bind_rows(lp_obj$constraint$content, sliceTbl)

  CONS_ID_start = nrow(lp_obj$constraint$content) + 1
  sliceTbl$CONS_ID = CONS_ID_start:(CONS_ID_start+nrow(sliceTbl)-1)

  lp_obj$constraint$content <- bind_rows_2(lp_obj$constraint$content, sliceTbl)
  # dplyr::full_join(lp_obj$constraint$content, sliceTbl, by = CONS_ID)

  return(invisible(lp_obj))

}
