
#' Read the passage slice from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains testInf and xlsx file for slice
#' @param input_obj the slice object
#' @return a LP object
#' @examples
#'   lp_obj = init_lp_obj(
#'     basic = list(
#'       test_name = "read_passage_slice example",
#'       n_form = 1),
#'     input = list(files = list(
#'       passage_slice_file = list(file = "inst/extdata/Psg01/passage_slice.xlsx")
#'     )),
#'     objective = c("passage_slice")
#'   )
#' @export
#'
read_passage_slice <- function (lp_obj, input_obj = NULL) {

  if (!is.null(input_obj)) {
    # read in pool information directly from a input_obj
    passageSliceTbl = mutate_col_type (input_obj, col_types = "icciicc")
  } else {
    file  = lp_obj$files$passage_slice_file$file
    sheet = lp_obj$files$passage_slice_file$sheet
    if (is.null(sheet)) sheet = 1

    if (!is.character(file)) {
      stop("No file is specified for the passage_slice")
    }

    if (is.xlsx.file(file)) {
      if (!file.exists(file)) {
        stop("Cannot find the file for the slice")
      }
                  # FORM      PSG_ID  UNIT    LOWER      UPPER      MARGIN  NOTE
      col_types = c("text","text", "text", "numeric", "numeric", "numeric", "text")
      passageSliceTbl = readxl::read_excel(file, col_types = col_types)
    } else {
      # c = character, i = integer, n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
      col_types = c("ccciiic")
      passageSliceTbl = read_gs (gs_file = file, sh = "alias", col_types = col_types)[[1]]
    }
  }

  # FORM ==> FORM_IND
  # 1. any NA in FORM is converted to 1:n_form
  # if (any(is.na(consTbl$FORM))) {
  #
  #   # stop("Error--FORM cannot contain empty cells!")
  #   n_form = sum(lp_obj$test_inf$n_form)
  #   consTbl$FORM[which(is.na(consTbl$FORM))] = paste0("1:", n_form)
  # }

  if (any(is.na(passageSliceTbl$FORM))) {
      # stop("Error--FORM cannot contain empty cells!")
      n_form = sum(lp_obj$test_inf$n_form)
      passageSliceTbl$FORM[which(is.na(passageSliceTbl$FORM))] = paste0("1:", n_form)
  }

  # 2. Add FORM_IND as a list of numeric column (e.g. "1:2" ==> c(1,2))
  passageSliceTbl <- passageSliceTbl %>%
    add_column(
      FORM_IND = lapply(passageSliceTbl$FORM, function (i) eval(parse(text = paste0("as.integer(c(", i, "))")))),
      .before = 1) %>%
    dplyr::select(-FORM)

  # 3. add MARGIN column if it does not exist
  if (!("MARGIN" %in% names(passageSliceTbl)))
  {
    passageSliceTbl$MARGIN = 0
  }

  # 4. convert NA to 0 for MARGIN
  if (any(is.na(passageSliceTbl$MARGIN)))
    passageSliceTbl$MARGIN[which(is.na(passageSliceTbl$MARGIN))] = as.integer(0)

  # 5. create passage.index

  items = lp_obj$items
  alias = lp_obj$alias

  # if (!(alias$psg.id %in% names(items))) stop("")

  if (all(is.na(items[[alias$psg_id]]))) return ("No passage ids exist in the pool.")

  passage_index = gen_passage_index (items, alias)

  # save to lp_obj

  lp_obj$constraint$passage_slice = passageSliceTbl
  lp_obj$passage_index = passage_index


  return(invisible(lp_obj))
}

gen_passage_index = function (items, alias) {

  psg.id = unique(items[[alias$psg_id]])
  psg.id = psg.id[!is.na(psg.id)]

  if (length(psg.id) <= 0) return ()

  return(
    tibble(psg.ind = seq(length(psg.id)),
           psg.id = psg.id))
}
