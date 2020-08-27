#' Write an lp object to input files
#'
#' @export


write_rds_to_xlsx <- function (lp_obj, save_to_folder) {

  require(writexl)
  require(stringr)

  obj_names = names(lp_obj$input_obj)

  if (length(obj_names) > 0)
    map(obj_names, ~{
      writexl::write_xlsx(lp_obj$input_obj[[.x]], str_c(save_to_folder, "/", .x, ".xlsx") )})
}

#' Write rds file to input files
#'
#' @export

write_rds_to_csv <- function (rds_file, save_to_folder) {

  require(readr)
  require(stringr)

  obj = readRDS(rds_file)

  if (!dir.exists(save_to_folder))
    dir.create(save_to_folder, T, T)

  if (length(obj) > 0)
    imap(obj,
      ~write_csv(.x, file = str_c(save_to_folder, "/", .y, ".csv")))
}


