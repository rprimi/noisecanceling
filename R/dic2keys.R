#' Build a scoring keys matrix from an item dictionary
#'
#' Converts an item dictionary into a scoring keys matrix in the format
#' expected by [psych::scoreItems()]. Each column is a scale; each cell is
#' `+1`, `-1` or `0` indicating how the item loads on that scale.
#'
#' @param item_dic A data frame with at least the columns `scale`, `pole` and
#'   `coditem`. An `order` column is added automatically if absent.
#' @param reversed Logical. If `TRUE` (the default), negatively keyed items are
#'   assumed to have already been reverse-scored, so all items keep a positive
#'   sign in the keys. If `FALSE`, negatively keyed items (`pole == 0`) are
#'   given a negative sign so [psych::scoreItems()] reverses them.
#'
#' @return A numeric matrix with one row per item and one column per scale.
#'
#' @keywords internal
#' @export
dic2keys <- function(item_dic, reversed = TRUE) {

  required_columns <- c("scale", "pole", "coditem")
  missing_cols <- setdiff(required_columns, names(item_dic))
  if (length(missing_cols) > 0) {
    stop("`item_dic` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!"order" %in% names(item_dic)) {
    item_dic$order <- seq_len(nrow(item_dic))
  }

  item_dic$order2 <- if (reversed) {
    item_dic$order
  } else {
    ifelse(item_dic$pole == 1, item_dic$order, item_dic$order * -1)
  }

  keys_list <- split(item_dic$order2, item_dic$scale)

  keys <- psych::make.keys(
    nvars = nrow(item_dic),
    keys.list = keys_list,
    item.labels = item_dic$coditem
  )

  as.matrix(keys[, unique(as.character(item_dic$scale))])
}
