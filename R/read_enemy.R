
#' Read the enemy groups from a xlsx/xls file or a google sheet file
#'
#' @param lp_obj the lp object contains testInf and xlsx file for enemy groups
#'
#' @param input_obj the enemy groups object
#'
#' @return a LP object
#'
#' @export
#'

read_enemy <- function (lp_obj, input_obj = NULL) {

  require(dplyr)
  if (!is.null(input_obj)) {

    # read in enemy groups information directly from an input_obj

    if (!is_tibble(input_obj)) input_obj = as_tibble(input_obj)
    enemy_tbl = input_obj

  } else {

    file  = lp_obj$files$enemy_file$file

    if (!is.character(file)) {

      stop("No file is specified for the enemy groups")

    }


    if (is.xlsx.file(file)) {

      if (!file.exists(file))

        stop("Cannot find the file for the enemy groups")

      sheet = lp_obj$files$enemy_file$sheet
      if (is.null(sheet)) sheet = 1

      # ENEMY_GROUP_ID	ENTITY_ID
      col_types = c("text","text")

      enemy_tbl = readxl::read_excel(file, sheet = sheet, col_types = col_types)

    } else {

      col_types = c("cc")
      # c = character, i = integer, n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
      enemy_tbl = read_gs (gs_file = file, sh = "enemy", col_types = col_types)[[1]]

    }
  }

  # add ITEM_IND column and convert long-style enemy_tbl to list style
  item_id_col_name = ifelse(is.null(lp_obj$alias) || is.null(lp_obj$alias$item_id), "item_id", lp_obj$alias$item_id)

  enemy_tbl$ITEM_IND = vapply(enemy_tbl[[item_id_col_name]], function(item_id) {
    which(lp_obj$items[item_id_col_name] == item_id)
  }, as.integer(0), USE.NAMES = FALSE)

  enemy_tbl = enemy_tbl %>% group_by(ENEMY_GROUP_ID) %>%
    summarise(
      !!sym(item_id_col_name) := list(!!sym(item_id_col_name)),
      ITEM_IND = list(ITEM_IND),
    .groups = "drop")

  # add enemy_tbl to lp_obj as $constraint$enemy

  lp_obj$constraint$enemy = enemy_tbl

  return(invisible(lp_obj))

}

#' convert _ENEMY_COL to many constraints using the item column "ENEMY"
#'
#' @param lp_obj the lp object
#'
#' @return a LP object
#'
#' @export
#'

get_ENEMY_COL_to_constraint <- function (lp_obj) {

  if (!("_ENEMY_COL" %in% lp_obj$constraint$content$ATTRIBUTE) || is.null(lp_obj$items$ENEMY))
    return(invisible(lp_obj))

  items = lp_obj$items
  enemy_groups = setdiff(unique(items$ENEMY), NA_character_) %>% as.character()

  enemy_cons = lp_obj$constraint$content %>%
    filter(ATTRIBUTE == "_ENEMY_COL")
  regular_cons = lp_obj$constraint$content %>%
    filter(ATTRIBUTE != "_ENEMY_COL")

  enemy_cons <- map_df(1:nrow(enemy_cons), ~{
      cons = enemy_cons[.x,]
      if (!is.na(cons$ATTRIBUTE_VALUE[1])) cons else {
        cons %>% mutate(ATTRIBUTE = "ENEMY",
                        ATTRIBUTE_VALUE = list(enemy_groups)) %>%
          unnest(cols = "ATTRIBUTE_VALUE") %>%
          mutate(CONS_ID = str_c(CONS_ID, "_ENEMY_", ATTRIBUTE_VALUE))
      }
    })

  lp_obj$constraint$content = bind_rows(regular_cons, enemy_cons)

  return(invisible(lp_obj))
}




