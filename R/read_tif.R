
#' Read the tif target from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains the tif file
#'
#' @param input_obj the tif object
#'
#' @return a LP object
#'
#' @export
#'

read_tif <- function (lp_obj, input_obj = NULL, verbose = F) {

  if (verbose) print("call read_tif()")

  if (!is.null(input_obj)) {

    # read in pool information directly from a input_obj

    # tifTbl = mutate_col_type (input_obj, col_types = "ccddddc")
    tifTbl = input_obj

  } else {

    file  = lp_obj$files$tif_file$file

    sheet = lp_obj$files$tif_file$sheet
    if (is.null(sheet)) sheet = 1

    nForm = lp_obj$test_inf$n_form

    if (!is.character(file)) {

      stop("No file is specified for the tif target")

    }

    if (is.xlsx.file(file)) {

      if (!file.exists(file)) {

        stop("Cannot find the file for the tif target")
      }

      # tifTbl = readxl::read_excel(file,  #TIF_ID	FORM	THETA	TIF_LOWER	TIF_UPPER	MARGIN	NOTE
      #                             col_types = c("text","text", "numeric", "numeric", "numeric",
      #                                           "numeric", "text"))
      tifTbl = readxl::read_excel(file)


    } else {

      # tifTbl = read_gs (gs_file = file, sh = "tif", col_types = c("ccddddc"))[[1]]
      tifTbl = read_gs (gs_file = file, sh = "tif")[[1]]

    }
  }

  if (verbose)  {
    print("tifTbl")
    print(tifTbl)
  }

  # 1. FORM cannot contain NA

  if (any(is.na(tifTbl$FORM))) {

    stop("Error--FORM cannot contain empty cells!")

  }

  # 2. make sure data type is correct

  tif_col_types = "ccd"
  if ("SCOPE" %in% names(tifTbl)) {
    tif_col_types = paste0(tif_col_types, "c")
  }
  tif_col_types = paste0(tif_col_types, "ddd")
  if ("MARGIN_WEIGHT" %in% names(tifTbl)) {
    tif_col_types = paste0(tif_col_types, "d")
  }
  tif_col_types = paste0(tif_col_types, "c")
  tifTbl <- mutate_col_type (tifTbl, col_types = tif_col_types)

  # 3. Add FORM_IND as a list of numeric column (e.g. "1:2" ==> c(1,2))

  tifTbl <- add_column(tifTbl,
                        FORM_IND = lapply(tifTbl$FORM, function (i) eval(parse(text = paste0("c(", i, ")")))),
                        .before = 1) %>% select (-FORM)

  # add tif target constraint to lp_obj

  lp_obj$constraint$tif = tifTbl

  return(invisible(lp_obj))

}

