#' Cut a numeric vector into quantile-based bins
#'
#' Splits a numeric vector into ordered groups defined by sample quantiles
#' (by default quartiles).
#'
#' @param x A numeric vector.
#' @param probs Numeric vector of probabilities defining the cut points.
#'   Defaults to quartiles, `c(0, .25, .5, .75, 1)`.
#' @param labels Character vector of labels for the resulting bins.
#'
#' @return A factor of the same length as `x`.
#' @export
#'
#' @examples
#' cria_quartis(rnorm(100))
cria_quartis <- function(x,
                         probs = c(0, .25, .50, .75, 1),
                         labels = c("<P25", "P25-P50", "P50-P75", ">P75")) {
  q <- stats::quantile(x, probs = probs, na.rm = TRUE)
  cut(x, q, ordered_result = FALSE, include.lowest = TRUE, labels = labels)
}

#' Map factor level codes to their labels
#'
#' @param x A factor.
#'
#' @return A data frame with the integer level codes (`levels`) and their
#'   string labels (`labels`).
#' @export
#'
#' @examples
#' map_factor_levels(factor(c("low", "high", "low")))
map_factor_levels <- function(x) {
  levels <- seq_along(levels(x))
  labels <- levels(x)
  data.frame(levels = levels, labels = labels, stringsAsFactors = FALSE)
}

#' Count pairwise complete cases between all column pairs
#'
#' For every pair of columns in a data frame, counts the number of rows with
#' no missing value on either column. Useful for inspecting the overlap of
#' responses across items.
#'
#' @param x A data frame.
#' @param ... Unused, for extensibility.
#'
#' @return A data frame with one row per column pair and the count of complete
#'   cases.
#' @export
#'
#' @examples
#' sharedcount(data.frame(a = c(1, NA, 3), b = c(1, 2, NA), c = 1:3))
sharedcount <- function(x, ...) {
  nx <- names(x)
  alln <- utils::combn(nx, 2)
  out <- apply(alln, 2, function(y) sum(stats::complete.cases(x[y])))
  data.frame(t(alln), out)
}
