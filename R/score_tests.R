#' Score tests and compute classical psychometrics
#'
#' Scores one or more scales from an item dictionary using
#' [psych::scoreItems()] (or [psych::scoreFast()]) and computes per-scale
#' reliability statistics. Unlike [find_psychometrics()], this function works
#' on a single data frame and does not require an [recode_for_acq()] object,
#' so it can be used to score either raw or acquiescence-recoded data.
#'
#' @param df A data frame of item responses.
#' @param item_dic Item dictionary with at least `coditem`, `scale` and `pole`
#'   columns. See [recode_for_acq()] for the expected format.
#' @param scr_tot Logical. Passed to `totals`: if `TRUE`, total (summed) scores
#'   are returned instead of means. Defaults to `FALSE`.
#' @param filename Path of the Excel file written when `save_item_stat = TRUE`.
#' @param save_item_stat Logical. If `TRUE`, item and scale statistics are
#'   written to `filename`. Defaults to `FALSE`.
#' @param reversed Logical. Passed to [dic2keys()]: whether negatively keyed
#'   items have already been reverse-scored. Defaults to `FALSE`.
#' @param score_fast Logical. If `TRUE`, use the faster [psych::scoreFast()]
#'   instead of [psych::scoreItems()]. Defaults to `FALSE`.
#' @param min_values,max_values Optional numeric vectors of the minimum and
#'   maximum possible value of each scored item. If `NULL` (the default) they
#'   are taken from the observed range of the scored items.
#'
#' @return A list with the scoring object (`psicom`), the `keys` matrix, the
#'   nested `alpha` results, and tidy `alpha_scale_stat` / `alpha_item_stat`
#'   data frames, plus the `item_dic`.
#'
#' @seealso [find_psychometrics()], [dic2keys()]
#' @export
#'
#' @examples
#' data(data_senna)
#' data(senna_dic)
#' res <- score_tests(data_senna, senna_dic)
#' res$alpha_scale_stat
score_tests <- function(df,
                        item_dic,
                        scr_tot = FALSE,
                        filename = "item_stats.xlsx",
                        save_item_stat = FALSE,
                        reversed = FALSE,
                        score_fast = FALSE,
                        min_values = NULL,
                        max_values = NULL) {

  keys <- dic2keys(item_dic, reversed)

  if (!all(rownames(keys) %in% colnames(df))) {
    stop("Not all items from the keys are present in `df`.", call. = FALSE)
  }

  items <- df[, rownames(keys), drop = FALSE]

  scoring_function <- if (score_fast) psych::scoreFast else psych::scoreItems
  score_args <- list(
    keys = keys,
    items = items,
    totals = scr_tot,
    missing = TRUE,
    impute = "none",
    delete = TRUE,
    digits = if (score_fast) 2 else 3
  )
  # Pass the response range only when explicitly supplied; otherwise psych
  # infers it from the observed data.
  if (!is.null(min_values)) score_args$min <- min_values
  if (!is.null(max_values)) score_args$max <- max_values

  psicom <- do.call(scoring_function, score_args)

  alpha <- alpha_by_scale(item_dic, responses = df)

  if (save_item_stat) {
    writexl::write_xlsx(
      x = list(
        alpha_item_stats = alpha$item_stat,
        alpha_scale_stat = alpha$scale_stat
      ),
      path = filename
    )
  }

  list(
    psicom = psicom,
    keys = keys,
    alpha_scale_stat = alpha$scale_stat,
    alpha_item_stat = alpha$item_stat,
    item_dic = item_dic
  )
}
