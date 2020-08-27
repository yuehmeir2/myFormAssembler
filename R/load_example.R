
#' Load an example (without solving it)
#'
#' @export
#'


load_example <- function (example_name = NULL, timeout = 60, solver = "cbc", gap = 0, verbose = TRUE) {

  if (is.null(example_name))
  {
    message(" -- Load Example 01\n")

    example_name = "Ex01"
  }

  example_path = system.file("extdata", package = "myFormAssembler")

  init.R = file.path(example_path, example_name, "init.R")

  if (!file.exists(init.R))
  {
    message("The available examples are:")

    as.message(dir(example_path))

    return(invisible())
  }

  lp_obj = source(init.R, local = TRUE)$value

  lp_obj$options$solver = solver

  lp_obj$options$timeout = timeout

  lp_obj$options$gap = gap

  lp_obj$options$verbose = verbose

  # read_alias, read_pool, read_constraint

  print("build_input_from_cat")
  if (!is.null(lp_obj$files$cat_input_file))
    lp_obj <- build_input_from_cat (lp_obj)

  print("read_input")
  if (length(setdiff(lp_obj$files, "cat_input_file")) > 0)
    lp_obj <- read_input(lp_obj)

  print("get_input")
  if (!is.null(lp_obj$input_obj))
    lp_obj <- get_input (lp_obj, lp_obj$input_obj, add_default_alias = FALSE)

  print("apply_template")
  if (!is.null(lp_obj$template) && !is.null(lp_obj$use_template))
    lp_obj <- apply_template(lp_obj)

  print("do_panel")
  if (!is.null(lp_obj$mst$n_panel) && lp_obj$mst$n_panel > 1)
    lp_obj = do_panel(lp_obj)

  return(invisible(lp_obj))

}
