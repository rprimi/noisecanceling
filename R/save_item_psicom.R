#' Export psychometric results to an Excel workbook
#'
#' Writes the results of [find_psychometrics()] to a multi-sheet Excel file,
#' combining item-level statistics, scale-level statistics and scale
#' correlations for both the original and the acquiescence-recoded scores.
#'
#' @param obj A list as returned by [find_psychometrics()].
#' @param filename Path of the `.xlsx` file to create.
#'
#' @return Invisibly returns `filename`. Called for the side effect of writing
#'   the workbook, which has the sheets `item_stats`, `scale_stats_ori`,
#'   `scale_stats_rec`, `scale_cor_o`, `scale_cor_r`, `alpha_ori` and
#'   `alpha_rec`.
#'
#' @seealso [find_psychometrics()], [save_item_psicom_si()]
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_senna)
#' data(senna_dic)
#' recoded <- recode_for_acq(data_senna, senna_dic)
#' psicom <- find_psychometrics(recoded, likert = 5, center = 3)
#' save_item_psicom(psicom, filename = tempfile(fileext = ".xlsx"))
#' }
save_item_psicom <- function(obj, filename) {

  item_stats_o <- as.data.frame(round(obj$psicom_orig$item.corrected, digits = 3))
  item_stats_r <- as.data.frame(round(obj$psicom_recoded$item.corrected, digits = 3))

  names(item_stats_o) <- paste(names(item_stats_o), "ori", sep = "_")
  names(item_stats_r) <- paste(names(item_stats_r), "rec", sep = "_")
  item_stats_o$coditem <- rownames(item_stats_o)
  item_stats_r$coditem <- rownames(item_stats_r)

  alpha_orig_item <- dplyr::rename_with(
    obj$alpha_orig_item_stat, ~ paste0(.x, "_ori")
  )
  alpha_rec_item <- dplyr::rename_with(
    obj$alpha_rec_item_stat, ~ paste0(.x, "_rec")
  )

  item_stats <- obj$item_dic %>%
    dplyr::left_join(item_stats_o, by = "coditem") %>%
    dplyr::left_join(
      alpha_orig_item,
      by = c("coditem" = "vars_ori", "scale" = "scale_ori")
    ) %>%
    dplyr::left_join(item_stats_r, by = "coditem") %>%
    dplyr::left_join(
      alpha_rec_item,
      by = c("coditem" = "vars_rec", "scale" = "scale_rec")
    )

  if (!is.null(obj$psicom_orig$response.freq)) {
    resp_frq <- as.data.frame(round(obj$psicom_orig$response.freq, digits = 3))
    resp_frq$coditem <- rownames(resp_frq)
    item_stats <- dplyr::left_join(item_stats, resp_frq, by = "coditem")
  }

  scale_stats_ori <- rbind(
    round(obj$psicom_orig$alpha, digits = 3),
    obj$psicom_orig$n.items,
    round(obj$psicom_orig$G6, digits = 3),
    t(psych::describe(obj$psicom_orig$scores))
  )
  scale_stats_rec <- rbind(
    round(obj$psicom_recoded$alpha, digits = 3),
    obj$psicom_recoded$n.items,
    round(obj$psicom_recoded$G6, digits = 3),
    t(psych::describe(obj$psicom_recoded$scores))
  )

  scale_cor_o <- round(obj$psicom_orig$cor, digits = 3)
  scale_cor_r <- round(obj$psicom_recoded$cor, digits = 3)

  writexl::write_xlsx(
    x = list(
      item_stats = as.data.frame(item_stats),
      scale_stats_ori = as.data.frame(scale_stats_ori),
      scale_stats_rec = as.data.frame(scale_stats_rec),
      scale_cor_o = as.data.frame(scale_cor_o),
      scale_cor_r = as.data.frame(scale_cor_r),
      alpha_ori = obj$alpha_orig_scale_stat,
      alpha_rec = obj$alpha_rec_scale_stat
    ),
    path = filename
  )

  invisible(filename)
}
