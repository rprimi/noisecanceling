#' Recode a data frame and compute the acquiescence index
#'
#' Estimates a per-person acquiescence index from a set of balanced (paired)
#' items and recodes every item response by subtracting that index. This is the
#' core "noise-cancelling" step: acquiescence is the noise that occurs with
#' both positively keyed (PK) and negatively keyed (NK) items, and centering
#' each response on the person's acquiescence index removes it.
#'
#' The acquiescence index is the within-person mean of all items that belong to
#' a semantic pair (a PK item and an NK item measuring the same content). With
#' balanced pairs, agreeing equally with an item and its opposite can only
#' reflect content-independent agreement, so that mean estimates acquiescence.
#'
#' @param data A data frame of raw item responses. Columns are items, rows are
#'   respondents. Must contain every item listed in `item_dic$coditem`.
#' @param item_dic A data frame describing the items (the item dictionary). One
#'   row per item, with at least these columns:
#'   \describe{
#'     \item{coditem}{Character. Column name of the item in `data`.}
#'     \item{scale}{Character. Name of the scale the item is scored on.}
#'     \item{pole}{Integer. `1` for a positively keyed (high pole) item, `0`
#'       for a negatively keyed (low pole) item.}
#'     \item{seman_pairs}{Integer or character identifying the semantic pair an
#'       item belongs to. Items with a non-missing value are treated as paired
#'       and used to compute the acquiescence index.}
#'     \item{item_text}{Character. The text of the item (optional, carried
#'       through for reporting).}
#'   }
#' @param acq_index_by_domain Logical. If `TRUE`, an acquiescence index and a
#'   within-subject standard deviation are also computed separately for each
#'   scale (using only scales with an even number of paired items). Defaults to
#'   `FALSE`.
#'
#' @return A list with five elements:
#'   \describe{
#'     \item{data}{The original item responses, restricted to dictionary items.}
#'     \item{data_acq_recoded}{Item responses centered on the acquiescence index.}
#'     \item{acq_index}{A data frame with the global acquiescence index
#'       (`acq_index`), the within-subject standard deviation (`ws_sd`) and, if
#'       `acq_index_by_domain = TRUE`, the per-scale indices.}
#'     \item{item_dic}{The item dictionary (column names lower-cased).}
#'     \item{item_dic_acq}{The dictionary restricted to paired items, with a
#'       `cntrst` contrast code (`+1` for PK, `-1` for NK).}
#'   }
#'
#' @seealso [find_psychometrics()] to analyse the recoded data.
#' @export
#'
#' @examples
#' data(data_senna)
#' data(senna_dic)
#' recoded <- recode_for_acq(data_senna, senna_dic)
#' head(recoded$acq_index)
recode_for_acq <- function(data, item_dic, acq_index_by_domain = FALSE) {

  names(item_dic) <- tolower(names(item_dic))

  required <- c("coditem", "scale", "pole", "seman_pairs")
  missing_cols <- setdiff(required, names(item_dic))
  if (length(missing_cols) > 0) {
    stop("`item_dic` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  item_dic$pole <- as.numeric(item_dic$pole)
  item_dic$seman_pairs <- as.numeric(item_dic$seman_pairs)

  v_tst <- unique(item_dic$coditem)
  missing_items <- setdiff(v_tst, names(data))
  if (length(missing_items) > 0) {
    stop("`data` is missing item column(s) listed in `item_dic`: ",
         paste(utils::head(missing_items, 10), collapse = ", "),
         if (length(missing_items) > 10) " ..." else "", call. = FALSE)
  }

  data <- data[, v_tst, drop = FALSE]

  item_dic_acq <- item_dic %>%
    dplyr::filter(!is.na(seman_pairs)) %>%
    dplyr::mutate(cntrst = ifelse(pole == 0, -1, 1))

  coditem_acq <- unique(item_dic_acq$coditem)
  if (length(coditem_acq) == 0) {
    stop("No paired items found: `seman_pairs` has no non-missing values.",
         call. = FALSE)
  }

  # Global acquiescence index and within-subject SD.
  acq_index <- rowMeans(data[, coditem_acq, drop = FALSE], na.rm = TRUE)
  ws_sd <- apply(data[, coditem_acq, drop = FALSE], 1, stats::sd, na.rm = TRUE)
  ws_sd <- ifelse(ws_sd == 0, NA, ws_sd)

  acq_index_df <- data.frame(acq_index = acq_index, ws_sd = ws_sd)

  # Per-scale indices (only scales with an even number of paired items).
  if (acq_index_by_domain) {
    by_scale <- split(
      item_dic_acq,
      factor(item_dic_acq$scale, levels = unique(item_dic_acq$scale))
    )
    acq_by_scale <- lapply(by_scale, function(x) {
      if (nrow(x) %% 2 == 0) {
        rowMeans(data[, x$coditem, drop = FALSE], na.rm = TRUE)
      }
    })
    sd_by_scale <- lapply(by_scale, function(x) {
      if (nrow(x) %% 2 == 0) {
        apply(data[, x$coditem, drop = FALSE], 1, stats::sd, na.rm = TRUE)
      }
    })
    acq_by_scale <- purrr::discard(acq_by_scale, is.null)
    sd_by_scale <- purrr::discard(sd_by_scale, is.null)
    if (length(acq_by_scale) > 0) {
      names(acq_by_scale) <- paste("acq", names(acq_by_scale), sep = "_")
      names(sd_by_scale) <- paste("ws_sd", names(sd_by_scale), sep = "_")
      acq_index_df <- cbind(
        acq_index_df,
        as.data.frame(acq_by_scale),
        as.data.frame(sd_by_scale)
      )
    }
  }

  # Centre every response on the person's acquiescence index.
  data_acq_recoded <- as.data.frame(
    apply(data[, v_tst, drop = FALSE], 2, function(x) x - acq_index)
  )

  list(
    data = data,
    data_acq_recoded = data_acq_recoded,
    acq_index = acq_index_df,
    item_dic = item_dic,
    item_dic_acq = item_dic_acq
  )
}
