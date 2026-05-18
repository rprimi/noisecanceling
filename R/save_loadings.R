#' Export factor loadings to an Excel workbook
#'
#' Extracts factor loadings from a [psych::fa()] or [psych::omega()] object and
#' appends them as a new sheet to an Excel workbook (creating the workbook if
#' it does not yet exist). For `fa` objects, item complexity and communality
#' are included; a `sort_load` column lets you sort items by loading in Excel.
#'
#' @param obj A factor-analysis object of class `fa` or `omega` (from the
#'   \pkg{psych} package).
#' @param item_dic Optional item dictionary; when supplied, item metadata is
#'   joined to the loadings by `coditem`.
#' @param filename Path of the `.xlsx` file. Created if it does not exist;
#'   otherwise a new sheet is appended.
#' @param digits Integer. Number of decimal places to round to. Defaults to `3`.
#' @param sheetName Name of the worksheet to add. Defaults to
#'   `"factor_loadings"`.
#'
#' @return Invisibly returns the data frame of loadings that was written.
#'
#' @seealso [write_sheet_in_excelfile()]
#' @export
save_loadings <- function(obj,
                          item_dic = NULL,
                          filename,
                          digits = 3,
                          sheetName = "factor_loadings") {

  is_omega <- inherits(obj, "omega")

  if (is_omega) {
    obj$loadings <- obj$schmid$sl
    dimnames(obj$loadings)[[1]] <-
      stringr::str_replace(dimnames(obj$loadings)[[1]], "-", "")
    dimnames(obj$schmid$sl)[[1]] <-
      stringr::str_replace(dimnames(obj$schmid$sl)[[1]], "-", "")
  }

  # Unsorted and sorted item orders, so the sheet can be re-sorted in Excel.
  order <- data.frame(
    coditem = as.character(dimnames(obj$loadings)[[1]]),
    stringsAsFactors = FALSE
  )
  if (is_omega) {
    order2 <- data.frame(
      coditem = as.character(dimnames(psych::fa.sort(obj)$schmid$sl)[[1]]),
      stringsAsFactors = FALSE
    )
  } else {
    order2 <- data.frame(
      coditem = as.character(dimnames(psych::fa.sort(obj)$loadings)[[1]]),
      stringsAsFactors = FALSE
    )
  }
  order2$sort_load <- as.numeric(rownames(order2))
  order <- dplyr::left_join(order, order2, by = "coditem")

  if (is_omega) {
    results <- data.frame(
      coditem = rownames(obj$loadings),
      round(unclass(obj$loadings), digits = digits),
      stringsAsFactors = FALSE
    )
  } else {
    results <- data.frame(
      coditem = rownames(obj$loadings),
      round(unclass(obj$loadings), digits = digits),
      comp = round(obj$complexity, digits = digits),
      h2 = round(obj$communality, digits = digits),
      stringsAsFactors = FALSE
    )
  }
  results <- dplyr::left_join(results, order, by = "coditem")

  if (!is.null(item_dic)) {
    names(item_dic) <- tolower(names(item_dic))
    results <- dplyr::left_join(results, item_dic, by = "coditem")
  }

  data_to_save <- if (inherits(obj, "fa") && obj$rotation != "varimax") {
    obj$Phi
  } else {
    results
  }

  write_sheet_in_excelfile(data_to_save, filename, sheetName)
  invisible(results)
}

#' Append a data frame as a new sheet in an Excel workbook
#'
#' Writes a data frame to a new worksheet, creating the workbook if it does not
#' yet exist and otherwise appending to it.
#'
#' @param df A data frame to write.
#' @param filename Path of the `.xlsx` file.
#' @param sheetName Name of the worksheet to add.
#'
#' @return Invisibly returns `filename`.
#' @keywords internal
#' @export
write_sheet_in_excelfile <- function(df, filename, sheetName) {
  wb <- if (file.exists(filename)) {
    openxlsx::loadWorkbook(filename)
  } else {
    openxlsx::createWorkbook()
  }
  openxlsx::addWorksheet(wb, sheetName)
  openxlsx::writeData(wb, sheet = sheetName, x = df)
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  invisible(filename)
}
