#' Compute classical psychometrics for original and acquiescence-recoded scores
#'
#' Takes the output of [recode_for_acq()], reverse-scores negatively keyed
#' items, and runs a classical psychometric analysis (scale scores, Cronbach's
#' alpha, item-total correlations) on both the original responses and the
#' acquiescence-recoded responses. Comparing the two shows how much
#' acquiescence inflated or distorted the original results.
#'
#' @param obj A list as returned by [recode_for_acq()].
#' @param likert Integer. The number of response categories of the Likert
#'   scale, used to reverse-score negatively keyed items in the original data
#'   (`likert + 1 - x`). Defaults to `5`.
#' @param center Numeric. Value added back to the centered responses after
#'   reverse-scoring, so recoded scores sit on an interpretable scale. Defaults
#'   to `0`.
#'
#' @return A list with:
#'   \describe{
#'     \item{scores}{Data frame of scale scores (original `_ori` and recoded
#'       `_rec`) together with the acquiescence indices.}
#'     \item{psicom_orig, psicom_recoded}{The [psych::scoreItems()] objects for
#'       the original and recoded data.}
#'     \item{alpha_orig_scale_stat, alpha_rec_scale_stat}{Per-scale reliability
#'       statistics.}
#'     \item{alpha_orig_item_stat, alpha_rec_item_stat}{Per-item statistics
#'       (including `r.drop`).}
#'     \item{item_dic}{The item dictionary.}
#'     \item{keys}{The scoring keys matrix.}
#'   }
#'
#' @seealso [recode_for_acq()], [save_item_psicom()]
#' @export
#'
#' @examples
#' data(data_senna)
#' data(senna_dic)
#' recoded <- recode_for_acq(data_senna, senna_dic)
#' psicom <- find_psychometrics(recoded, likert = 5, center = 3)
#' psicom$alpha_orig_scale_stat
find_psychometrics <- function(obj, likert = 5, center = 0) {

  item_dic <- obj$item_dic

  # Pole of each item, in the column order of the response data.
  pole_lookup <- item_dic[!duplicated(item_dic$coditem), c("coditem", "pole")]
  pole <- pole_lookup$pole[match(names(obj$data), pole_lookup$coditem)]
  nk <- which(pole == 0)
  pk <- which(pole == 1)

  # Reverse negatively keyed items and recenter.
  obj$data_acq_recoded[nk] <- (-1 * obj$data_acq_recoded[nk]) + center
  obj$data_acq_recoded[pk] <- obj$data_acq_recoded[pk] + center
  obj$data[nk] <- likert + 1 - obj$data[nk]

  keys <- dic2keys(item_dic)

  # Original scores.
  psicom_orig <- psych::scoreItems(
    keys = keys,
    items = obj$data[, rownames(keys), drop = FALSE],
    missing = TRUE, impute = "none",
    digits = 3
  )

  # Acquiescence-recoded scores.
  psicom_recoded <- psych::scoreItems(
    keys = keys,
    items = obj$data_acq_recoded[, rownames(keys), drop = FALSE],
    missing = TRUE, impute = "none"
  )

  colnames(psicom_orig$scores) <-
    paste(colnames(psicom_orig$scores), "ori", sep = "_")
  colnames(psicom_recoded$scores) <-
    paste(colnames(psicom_recoded$scores), "rec", sep = "_")

  alpha_orig <- alpha_by_scale(item_dic, responses = obj$data)
  alpha_rec <- alpha_by_scale(item_dic, responses = obj$data_acq_recoded)

  scores <- as.data.frame(cbind(
    psicom_orig$scores,
    psicom_recoded$scores,
    obj$acq_index
  ))

  list(
    scores = scores,
    psicom_orig = psicom_orig,
    psicom_recoded = psicom_recoded,
    alpha_orig_scale_stat = alpha_orig$scale_stat,
    alpha_orig_item_stat = alpha_orig$item_stat,
    alpha_rec_scale_stat = alpha_rec$scale_stat,
    alpha_rec_item_stat = alpha_rec$item_stat,
    item_dic = item_dic,
    keys = keys
  )
}

#' Per-scale reliability via psych::alpha
#'
#' Internal helper that runs [psych::alpha()] for every scale and returns tidy
#' scale-level and item-level statistics. Items are assumed to be already
#' reverse-scored.
#'
#' @param item_dic Item dictionary with `coditem` and `scale` columns.
#' @param responses Data frame of (reverse-scored) item responses.
#'
#' @return A list with `scale_stat` and `item_stat` data frames.
#' @keywords internal
#' @noRd
alpha_by_scale <- function(item_dic, responses) {
  nested <- item_dic %>%
    dplyr::select(coditem, scale) %>%
    dplyr::group_by(scale) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      vars = purrr::map(data, "coditem"),
      alfa = purrr::map(
        vars,
        ~ psych::alpha(x = responses[, .x], warnings = FALSE)
      )
    ) %>%
    dplyr::mutate(
      scale_stat = purrr::map(alfa, "total"),
      item_stats = purrr::map(alfa, "item.stats")
    )

  scale_stat <- nested %>%
    dplyr::select(scale, scale_stat) %>%
    tidyr::unnest_wider(scale_stat) %>%
    dplyr::ungroup()

  item_stat <- nested %>%
    dplyr::select(scale, vars, item_stats) %>%
    tidyr::unnest(cols = c(vars, item_stats)) %>%
    dplyr::ungroup()

  list(scale_stat = scale_stat, item_stat = item_stat)
}
