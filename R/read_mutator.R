
#' Read the mutators from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains xlsx file for mutators
#'
#' @param input_obj the mutators object
#'
#' @return a LP object
#'
#' @export
#'

read_mutator <- function (lp_obj, input_obj = NULL) {

  require(dplyr)
  if (!is.null(input_obj)) {

    # read in mutators information directly from an input_obj

    if (!is_tibble(input_obj)) input_obj = as_tibble(input_obj)
    mutator_tbl = input_obj

  } else {

    file  = lp_obj$files$mutator_file$file

    if (!is.character(file)) {

      stop("No file is specified for the mutators")

    }


    if (is.xlsx.file(file)) {

      if (!file.exists(file))

        stop("Cannot find the file for the mutators")

      sheet = lp_obj$files$mutator_file$sheet
      if (is.null(sheet)) sheet = 1

      # CONS_ID	PROB	LABEL
      col_types = c("text","numeric","text")

      mutator_tbl = readxl::read_excel(file, sheet = sheet, col_types = col_types)

    } else {

      col_types = c("cdc")
      # c = character, i = integer, n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
      mutator_tbl = read_gs (gs_file = file, sh = "mutator", col_types = col_types)[[1]]

    }
  }

  # add mutator_tbl to lp_obj as $constraint$mutator

  lp_obj$constraint$mutator = mutator_tbl

  return(invisible(lp_obj))

}

