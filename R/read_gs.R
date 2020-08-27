#' Read in google sheets from a given title
#'
#' @param gs_file
#' the gsheet file name
#'
#' @param sh
#' a vector of positive integer or character string specifying index or title.
#' use sh == 0 to get all sheets
#'
#' @return a list of tibbles for the sheets requested
#'
#' @export
#'

read_gs = function (gs_file, sh, ...) {

  require(googlesheets)

  # register for assessing googlesheets

  titles = gs_ls()$sheet_title

  # check the file exists

  if (!(gs_file %in% titles)) return ()

  # access to the gsheet file
  gfile <- gs_title(gs_file)

  # list worksheets
  gsheets = gs_ws_ls(gfile)

  # get the sh index

  if (is.character(sh)) {

    sh_ind = unlist(lapply(sh, function (s) which(toupper(gsheets) %in% toupper(s))))
    sh_names = sh

  }
  else {

    if (sh == 0) sh_ind = seq(length(gsheets)) else sh_ind = sh[which(sh <= length(gsheets))]
    sh_names = gsheets[sh_ind]

  }

  sheets = lapply(sh_ind, function (s) gs_read(ss = gfile, ws = s, ... ))
  names(sheets) = sh_names

  return (sheets)
}
