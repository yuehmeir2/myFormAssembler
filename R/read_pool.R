
#' Read items from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains testInf and xlsx file for items
#'
#' @param input_obj the item/pool object
#'
#' @return a lp object
#'
#' @export
#'

read_pool <- function (lp_obj, input_obj = NULL) {

  if (!is.null(input_obj)) {

    # read in pool information directly from a input_obj

    if (!is_tibble(input_obj)) input_obj = as_tibble(input_obj)
    ip = input_obj

  } else {

    file  = lp_obj$files$pool_file$file

    sheet = lp_obj$files$pool_file$sheet

    if (is.null(sheet)) sheet = 1

    if (!is.character(file))
    {
      stop("No file is specified for the constraint")

    }

    if (is.xlsx.file(file)) {

        if (!file.exists(file))
        {
            stop("Cannot find the file for the constraint")
        }

        # read in pool

        ip = readxl::read_excel(file)

    } else { # google sheets

        ip = read_gs (gs_file = file, sh = "item")[[1]]

    }
  }

  # check ip's column names against the alias

  alias = lp_obj$alias
  # print(alias)

  w = which(!(unlist(alias) %in% names(ip)))

  if (length(w) > 0) {

    message("These alias names in the alias table are not in the pool.\n")

    as.message(unlist(alias)[w])

    message("")

    stop ("Some alias names in the alias table cannot be found in the pool.")

  }

  # basic checking for item ids

  itemIds = ip[[alias$item]]

  w = which(duplicated(itemIds))

  if (length(w) > 0) {

    message("The following item IDs are duplicated:\n")

    as.message(itemIds[w])

    stop("Item IDs must be unique")
  }

  if (any(is.na(itemIds))) {

    stop("Item IDs must not be empty")

  }

  lp_obj$items = ip

  return(invisible(lp_obj))

}



