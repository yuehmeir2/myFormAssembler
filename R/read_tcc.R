
#' Read the tcc target from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains the tcc file
#'
#' @param input_obj the tcc object
#'
#' @return a LP object
#'
#' @export
#'

read_tcc <- function (lp_obj, input_obj = NULL, verbose = FALSE) {

  if (verbose) print ("call read_tcc()")

  if (!is.null(input_obj)) {

    # read in pool information directly from a input_obj

    # tccTbl = mutate_col_type (input_obj, col_types = "ccddddc")
    tccTbl = input_obj

  } else {

    file  = lp_obj$files$tcc_file$file

    sheet = lp_obj$files$tcc_file$sheet
    if (is.null(sheet)) sheet = 1

    nForm = lp_obj$test_inf$n_form

    if (!is.character(file)) {

      stop("No file is specified for the tcc target")

    }


    if (is.xlsx.file(file)) {

      if (!file.exists(file)) {

        stop("Cannot find the file for the tcc target")
      }

      # tccTbl = readxl::read_excel(file,  #TCC_ID	FORM	THETA	TCC_LOWER	TCC_UPPER	MARGIN	NOTE
      #                             col_types = c("text","text", "numeric", "numeric", "numeric",
      #                                           "numeric", "text"))
      tccTbl = readxl::read_excel(file)

    } else {

      # tccTbl = read_gs (gs_file = file, sh = "tcc", col_types = "ccddddc")[[1]]
      tccTbl = read_gs (gs_file = file, sh = "tcc")[[1]]

    }
  }

  if (verbose)  {
    print("tccTbl")
    print(tccTbl)
  }

  # 1. FORM cannot contain NA

  if (any(is.na(tccTbl$FORM))) {

    stop("Error--FORM cannot contain empty cells!")

  }

  # 2. make sure data type is correct
  # TCC_ID	FORM	THETA	TCC_LOWER	TCC_UPPER	MARGIN	NOTE

  if ("SCOPE" %in% names(tccTbl)) tccTbl <- mutate_col_type (tccTbl, col_types = "ccdcdddc") else
                                  tccTbl <- mutate_col_type (tccTbl, col_types = "ccddddc")

  # 3. Add FORM_IND as a list of numeric column (e.g. "1:2" ==> c(1,2)) and drop FORM

  tccTbl <- add_column(tccTbl,
                       FORM_IND = lapply(tccTbl$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
                       .before = 1) %>% select (-FORM)

  # add tcc target constraint to lp_obj

  lp_obj$constraint$tcc = tccTbl

  return(invisible(lp_obj))

}

