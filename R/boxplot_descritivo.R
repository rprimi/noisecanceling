#' Faceted boxplots of scale scores with standardized regressions
#'
#' Draws faceted boxplots of one or more scale scores against a grouping
#' variable and, for each scale, fits a linear model predicting the scale score
#' from a predictor and a grouping variable, returning the standardized
#' regression coefficients and R-squared.
#'
#' @param df A data frame containing the scale scores and the grouping
#'   variables.
#' @param scales Character vector of scale-score column names in `df`.
#' @param x_var Name of the variable on the x axis (and a model predictor).
#' @param grid_x_var Name of the variable used for row facetting (and the
#'   second model predictor).
#' @param breaks Numeric vector of y-axis breaks.
#' @param intercept Numeric. Where to draw a horizontal reference line.
#'
#' @return A list with `plot` (a \pkg{ggplot2} object) and `regressions` (a
#'   data frame of standardized betas and R-squared, one row per scale).
#' @export
boxplot_descritivo <- function(df, scales, x_var, grid_x_var, breaks,
                               intercept) {

  if (!requireNamespace("lm.beta", quietly = TRUE)) {
    stop("Package 'lm.beta' is required for boxplot_descritivo(). ",
         "Install it with install.packages('lm.beta').", call. = FALSE)
  }

  plot <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(scales, x_var, grid_x_var))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(scales),
      names_to = "domain", values_to = "score"
    ) %>%
    dplyr::filter(
      !is.na(score), !is.na(domain),
      !is.na(.data[[x_var]]), !is.na(.data[[grid_x_var]])
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(y = score, x = .data[[x_var]], fill = domain)
    ) +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::facet_grid(stats::reformulate(domain, response = grid_x_var)) +
    ggplot2::scale_y_continuous(breaks = breaks) +
    ggplot2::geom_hline(yintercept = intercept)

  regressions <- lapply(scales, function(scale) {
    fit <- stats::lm(
      stats::as.formula(paste0(scale, " ~ ", x_var, " + ", grid_x_var)),
      data = df
    )
    std <- summary(lm.beta::lm.beta(fit))$coefficients[, "Standardized", drop = TRUE]
    out <- c(r_squared = summary(fit)$r.squared, std)
    data.frame(
      scale = scale,
      term = names(out),
      estimate = as.numeric(out),
      stringsAsFactors = FALSE
    )
  })

  list(
    plot = plot,
    regressions = do.call(rbind, regressions)
  )
}
