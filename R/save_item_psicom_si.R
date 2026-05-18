#' Export single-set psychometric results to an Excel workbook
#'
#' A simplified counterpart of [save_item_psicom()] for results that have a
#' single set of scores (no original/recoded split), such as the output of
#' [score_tests()]. Writes item statistics, item-level alpha statistics,
#' scale statistics and the scale correlation matrix.
#'
#' @param obj A list with `psicom`, `alpha_item_stat`, `alpha_scale_stat` and
#'   `item_dic` elements, as returned by [score_tests()].
#' @param filename Path of the `.xlsx` file to create.
#'
#' @return Invisibly returns `filename`. Called for the side effect of writing
#'   a workbook with the sheets `item_stats`, `alpha_item_stat`, `scale_stat`
#'   and `scale_cor`.
#'
#' @seealso [score_tests()], [save_item_psicom()]
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_senna)
#' data(senna_dic)
#' res <- score_tests(data_senna, senna_dic)
#' save_item_psicom_si(res, filename = tempfile(fileext = ".xlsx"))
#' }
save_item_psicom_si <- function(obj, filename) {

  item_stats <- as.data.frame(round(obj$psicom$item.corrected, digits = 3))
  item_stats$coditem <- rownames(item_stats)

  item_stats <- obj$item_dic %>%
    dplyr::left_join(item_stats, by = "coditem") %>%
    dplyr::left_join(
      obj$alpha_item_stat,
      by = c("coditem" = "vars", "scale" = "scale")
    )

  alpha_item_stat <- dplyr::left_join(
    obj$item_dic, obj$alpha_item_stat,
    by = c("coditem" = "vars", "scale" = "scale")
  )

  if (!is.null(obj$psicom$response.freq)) {
    resp_frq <- as.data.frame(round(obj$psicom$response.freq, digits = 3))
    resp_frq$coditem <- rownames(resp_frq)
    item_stats <- dplyr::left_join(item_stats, resp_frq, by = "coditem")
  }

  scale_stat <- rbind(
    round(obj$psicom$alpha, digits = 3),
    obj$psicom$n.items,
    round(obj$psicom$G6, digits = 3),
    t(psych::describe(obj$psicom$scores))
  )
  stats <- c("alfa", "n.items", "G6", names(psych::describe(obj$psicom$scores)))
  scale_stat <- dplyr::bind_cols(
    data.frame(statistics = stats),
    as.data.frame(scale_stat)
  )

  scale_cor <- round(obj$psicom$cor, digits = 3)

  writexl::write_xlsx(
    x = list(
      item_stats = as.data.frame(item_stats),
      alpha_item_stat = as.data.frame(alpha_item_stat),
      scale_stat = as.data.frame(scale_stat),
      scale_cor = as.data.frame(scale_cor)
    ),
    path = filename
  )

  invisible(filename)
}
