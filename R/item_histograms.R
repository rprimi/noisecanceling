#' Plot item response distributions by scale and pole
#'
#' Builds a faceted bar chart of the proportion of respondents choosing each
#' response category, with one facet per scale and per item pole. Useful for
#' inspecting whether positively and negatively keyed items behave symmetrically
#' before correcting for acquiescence.
#'
#' @param df A data frame of item responses.
#' @param coditems Character vector of item column names in `df`.
#' @param scales Character vector (same length as `coditems`) giving the scale
#'   of each item.
#' @param poles Vector (same length as `coditems`) giving the pole of each item
#'   (`1` positively keyed, `0` negatively keyed).
#' @param r_levels Vector of the ordered response categories (e.g. `1:5`).
#'
#' @return A \pkg{ggplot2} object.
#' @export
#'
#' @examples
#' data(data_senna)
#' data(senna_dic)
#' d <- senna_dic[senna_dic$scale == "C", ]
#' item_histograms(data_senna, d$coditem, d$scale, d$pole, r_levels = 1:5)
item_histograms <- function(df, coditems, scales, poles, r_levels) {

  dic <- data.frame(
    coditem = coditems,
    scale = scales,
    pole = poles,
    stringsAsFactors = FALSE
  )

  df %>%
    dplyr::select(dplyr::all_of(coditems)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(coditems),
      names_to = "coditem", values_to = "r"
    ) %>%
    dplyr::left_join(dic, by = "coditem") %>%
    dplyr::filter(!is.na(r)) %>%
    dplyr::group_by(scale, pole, r) %>%
    dplyr::tally(name = "freq") %>%
    dplyr::mutate(
      pole = factor(pole),
      tot = sum(freq),
      prop = freq / tot,
      r = factor(r, levels = r_levels)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(y = prop, x = r, fill = scale)) +
    ggplot2::geom_col(alpha = 0.5, color = "darkgray") +
    ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::facet_grid(pole ~ scale) +
    ggplot2::theme_minimal()
}
