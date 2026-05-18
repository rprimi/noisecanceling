#' Diverging bar plot of Likert item responses
#'
#' Produces a centered ("diverging") stacked bar plot of Likert item responses
#' using the \pkg{likert} package, with items ordered by their location
#' (e.g. a Thurstone threshold). Low, neutral and high response categories are
#' shown in distinct colors.
#'
#' @param data A data frame of raw item responses (one column per item).
#' @param coditem Character vector of item codes.
#' @param item_text Character vector of item labels (used on the y axis).
#' @param pole Vector of item poles (`1` positively keyed, `0` negatively
#'   keyed).
#' @param item_location Numeric vector of item locations used to order items.
#' @param item_text_max Integer. Maximum width of item labels before wrapping.
#'   Defaults to `28`.
#' @param center Numeric. The neutral response category. Defaults to `3`.
#' @param categ_levels Character vector of the response category codes.
#' @param categ_labels Character vector of the response category labels.
#' @param low.color,neutral.color,high.color Fill colors for the low, neutral
#'   and high response categories.
#'
#' @return A \pkg{ggplot2} object.
#' @export
describe_likert <- function(data,
                            coditem,
                            item_text,
                            pole,
                            item_location,
                            item_text_max = 28,
                            center = 3,
                            categ_levels = c("1", "2", "3", "4", "5"),
                            categ_labels = c("nada", "pouco", "moderad.",
                                             "muit.", "totalm."),
                            low.color = "#DF4949",
                            neutral.color = "#EEE657",
                            high.color = "#2CCA90") {

  if (!requireNamespace("likert", quietly = TRUE)) {
    stop("Package 'likert' is required for describe_likert(). ",
         "Install it with install.packages('likert').", call. = FALSE)
  }

  dic <- data.frame(
    coditem = coditem,
    item_text = item_text,
    pole = pole,
    b = item_location,
    stringsAsFactors = FALSE
  )

  n_levels <- length(categ_levels)
  data <- purrr::map_df(data, factor, levels = seq_len(n_levels))
  names(data) <- dic$item_text

  table_summary <- likert::likert(as.data.frame(data), nlevels = n_levels)
  table_summary$results$Item <- factor(
    table_summary$results$Item,
    levels = dic$item_text[order(dic$b)]
  )

  plot(
    table_summary,
    centered = TRUE, center = center, include.center = TRUE,
    wrap = item_text_max,
    low.color = low.color,
    neutral.color = neutral.color,
    high.color = high.color,
    ordered = FALSE
  ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}
