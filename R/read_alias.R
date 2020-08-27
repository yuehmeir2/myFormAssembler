
#' Read alias from a file or from an tibble variable
#'
#' @param lp_obj the lp object
#'
#' @param input_obj the alias object
#'
#' @return a lp object
#'
#' @export
#'

read_alias <- function (lp_obj, input_obj = NULL) {

  if (!is.null(input_obj)) {

    # read in alias information directly from a input_obj

    data <- mutate_col_type (input_obj, col_types = "cc") %>% set_names(c("attribute", "alias"))

  } else {

    # read in alias information from a file

    file  = lp_obj$files$alias_file$file

    if (is.null(file)) {

      return ("No alias file is provided!")

    }

    if (is.xlsx.file(file)) {

        if (!file.exists(file)) {

          stop("Cannot find the file for the alias")
        }

        # xlsx or xls file

        sheet = lp_obj$files$alias_file$sheet

        if (is.null(sheet)) sheet = 1

        data = readxl::read_excel(file, sheet = sheet, col_types = c("text","text"))

    } else { # google sheets

        data = read_gs (gs_file = file, sh = "alias", col_types = c("cc"))[[1]]

    }
  }

  n_alias = sum(!is.na(data$attribute))

  alias = as.list(data$alias[1:n_alias])

  names(alias) = data$attribute[1:n_alias]

  if (nrow(data) > n_alias)
    alias$others = data$alias[(n_alias+1):nrow(data)]

  lp_obj$alias = alias


  return(invisible(lp_obj))

}


#' convert the alias variable in the list format to a tibble format (original format)
#' that can be shown by a editDT
#'
#' @param alias_list the alias list that has a variable "others" for all fields not initially needed
#' by the ATA
#'
#' @return the alias in tibble format
#'
#' @export
#'

aliasListToTibble <- function (alias_list) {

  needed_alias = setdiff(names(alias_list), "others")

  # attribute	alias
  alias = tibble(attribute = needed_alias, alias = sapply(needed_alias, function(i) alias_list[[i]]))
  if ("others" %in% names(alias_list)) {
    others = alias_list$others
    alias <- tibble::add_row(alias, attribute = rep(NA, length(others)), alias = others)
  }
  return (invisible(alias))
}
