# Internal: "Senna Kids" acquiescence pipeline.
#
# These functions implement an alternative acquiescence model for a
# binary-plus-rating response format (a response such as "D2" combines a
# binary pole choice with a 1-3 rating). They are kept internal: the headline
# package interface is the mean-centering model in recode_for_acq().

#' Reshape wide item data to long format with an item dictionary
#'
#' @param df Wide data frame of item responses.
#' @param id Name of the respondent id column.
#' @param coditems,scales,poles,item_text,seman_pairs Vectors describing the
#'   items, used to build the item dictionary that is joined to the long data.
#'
#' @return A long data frame with one row per respondent-item.
#' @keywords internal
#' @noRd
create_df_long <- function(df, id = "codsuj", coditems, scales, poles,
                           item_text, seman_pairs) {
  dic <- data.frame(
    coditem = coditems,
    scale = scales,
    pole = poles,
    item_text = item_text,
    seman_pairs = seman_pairs,
    stringsAsFactors = FALSE
  )

  df %>%
    dplyr::select(dplyr::all_of(c(id, coditems))) %>%
    tidyr::pivot_longer(
      dplyr::all_of(coditems),
      names_to = "coditem", values_to = "r"
    ) %>%
    dplyr::left_join(dic, by = "coditem")
}

#' Compute acquiescence indexes for the binary-plus-rating format
#'
#' @param df A long data frame as returned by `create_df_long()`.
#' @param id Name of the respondent id column.
#'
#' @return A list with per-person acquiescence scores (`acq_indx_scores`) and
#'   the intermediate pair-level data (`acq_indx_df`).
#' @keywords internal
#' @noRd
compute_acq_indexes <- function(df, id = "ExamineeID") {
  acq_indx_df <- df %>%
    dplyr::filter(!is.na(r), !is.na(seman_pairs)) %>%
    tidyr::separate(col = r, into = c("bin", "rating"), sep = 1) %>%
    dplyr::arrange(.data[[id]], seman_pairs, pole) %>%
    dplyr::group_by(.data[[id]], seman_pairs) %>%
    dplyr::mutate(
      r_bin_pair = paste(bin, collapse = ""),
      pair_pole = paste(pole, collapse = "")
    ) %>%
    dplyr::filter(length(r_bin_pair) == 2, length(pair_pole) == 2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      acq_index_bin =
        as.integer(pair_pole == "01") *
          (as.integer(r_bin_pair == "DD") + as.integer(r_bin_pair == "EE")) +
        as.integer(pair_pole != "01") *
          (as.integer(r_bin_pair == "DE") + as.integer(r_bin_pair == "ED"))
    )

  acq_indx_scores <- acq_indx_df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::summarise(
      n_pairs = dplyr::n(),
      acq_index_bin_mean = mean(acq_index_bin, na.rm = TRUE),
      acq_index = mean(acq_index_bin * as.integer(rating), na.rm = TRUE),
      resp_D_mean = mean(bin == "D", na.rm = TRUE),
      .groups = "drop"
    )

  list(acq_indx_scores = acq_indx_scores, acq_indx_df = acq_indx_df)
}

#' Recode and score the binary-plus-rating format, correcting for acquiescence
#'
#' @param df_long A long data frame as returned by `create_df_long()`.
#' @param acq_scores Per-person acquiescence scores from `compute_acq_indexes()`.
#' @param id Name of the respondent id column.
#' @param scales Name of the scale column.
#'
#' @return A list with the recoded long data (`df_long`) and the wide
#'   per-person scale `scores`.
#' @keywords internal
#' @noRd
recode_and_score_sennakids <- function(df_long, acq_scores,
                                       id = "ExamineeID", scales = "scale") {
  df_long <- df_long %>%
    dplyr::left_join(acq_scores, by = id) %>%
    tidyr::separate(col = r, into = c("bin", "rating"), sep = 1) %>%
    dplyr::mutate(
      rating = as.integer(rating),
      bin_scr_D = ifelse(bin == "D", 1, 0),
      bin_scr_rev = ifelse(pole == 0, 1 - bin_scr_D, bin_scr_D),
      r_1_6 = (bin_scr_D * 3) + rating + 1,
      cnst_inversao = ifelse((bin_scr_D + pole) == 1, 4, 0),
      parte_456 = bin_scr_rev * 3 + 1 + rating,
      r_1_6_rev = abs(cnst_inversao - parte_456),
      acq_index_recentered = acq_index - 1,
      rating_recoded = rating + acq_index_recentered,
      parte_456_rec = bin_scr_rev * 3 + rating_recoded + 1,
      r_1_6_rev_rec = abs(cnst_inversao - parte_456_rec)
    )

  scores <- df_long %>%
    dplyr::group_by(.data[[id]], .data[[scales]]) %>%
    dplyr::summarise(
      acq_index = mean(acq_index),
      acq_index_recentered = mean(acq_index_recentered),
      bin_scr_rev = mean(bin_scr_rev),
      r_1_6_rev = mean(r_1_6_rev),
      r_1_6_rev_rec = mean(r_1_6_rev_rec),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(.data[[id]], acq_index, acq_index_recentered),
      names_from = .data[[scales]],
      values_from = c(bin_scr_rev, r_1_6_rev, r_1_6_rev_rec)
    )

  list(df_long = df_long, scores = scores)
}
