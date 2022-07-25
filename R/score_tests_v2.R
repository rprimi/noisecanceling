score_tests_v2 <- function(df,
                           item_dic,
                           scr_tot = FALSE,
                           filename = "item.stats.xlsx",
                           save_item_stat = FALSE,
                           reversed = FALSE,
                           score_fast = FALSE) {

  # expects: "coditem", "domain", "pole", "seman_pairs"
  # pole: 0 negatively keyed and 1 positively keyed

  require(plyr)
  require(psych)
  require(xlsx)
  require(dplyr)
  require(purrr)

  # make keys
  keys <- dic2keys(item_dic, reversed)

  # original
  min <- apply(df[, rownames(keys)], 2, function(x) {
    min(x, na.rm = TRUE)
  })
  max <- apply(df[, rownames(keys)], 2, function(x) {
    max(x, na.rm = TRUE)
  })
  if (score_fast) {
    obj <- scoreFast(
      keys = keys,
      items = df[, rownames(keys)],
      totals = scr_tot,
      missing = TRUE,
      impute = "none",
      delete = TRUE,
      min = min,
      max = max,
      digits = 2
    )
  } else {
    obj <- scoreItems(
      keys = keys,
      min = min,
      max = max,
      totals = scr_tot,
      items = df[, rownames(keys)],
      missing = TRUE,
      impute = "none",
      digits = 3
    )
  }

  # Calculate psychometrics via alpha function to get r.drop

  # Original
  alpha_orig <- item_dic %>%
    dplyr::mutate(pole2 = ifelse(pole == 0, -1, 1)) %>%
    dplyr::select(coditem, scale, pole2) %>%
    group_by(scale) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      vars = purrr::map(data, "coditem"),
      keys = purrr::map(data, "pole2")
    ) %>%
    dplyr::mutate(
      alfa = purrr::map(vars, ~ psych::alpha(x = df[, .x]))
    ) %>%
    dplyr::mutate(
      scale_stat = map(alfa, "total"),
      item_stats = map(alfa, "item.stats")
    )

  alpha_orig_scale_stat <- alpha_orig %>%
    dplyr::select(scale, scale_stat) %>%
    tidyr::unnest_wider(scale_stat) %>%
    dplyr::ungroup()

  alpha_orig_item_stat <- alpha_orig %>%
    dplyr::select(scale, vars, item_stats) %>%
    tidyr::unnest(cols = c(vars, item_stats)) %>%
    dplyr::ungroup()


  # Save item stats

  if (save_item_stat) {
    item_cor <- as.data.frame(round(obj$item.corrected, digits = 3))
    # por causa de um pau no psych
    item_stats <- as.data.frame(round(obj$item.corrected, digits = 3))
    item_stats$coditem <- rownames(item_stats)


    item_stats <- item_dic %>%
      dplyr::left_join(item_stats, by = "coditem") %>%
      dplyr::left_join(alpha_orig_item_stat, by = c("coditem" = "vars", "scale"))

    if (!is.null(obj$response.freq)) {
      resp_frq <- as.data.frame(
        round(obj$response.freq, digits = 3)
      ) %>%
        mutate(coditem = rownames(.))
      item_stats <- item_stats %>%
        left_join(resp_frq, by = "coditem")
    }

    scale_stats <- rbind(
      round(obj$alpha, digits = 3),
      obj$n.items,
      round(obj$G6, digits = 3),
      t(describe(obj$scores))
    )

    scale_cor <- round(obj$cor, digits = 3)

    write.xlsx(item_stats, filename,
      sheetName = "item_stats",
      col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE
    )

    write.xlsx(scale_stats, filename,
      sheetName = "scale_stats",
      col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE
    )

    write.xlsx(scale_cor, filename,
      sheetName = "scale_cor",
      col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE
    )

    write.xlsx(alpha_orig_scale_stat, filename,
      sheetName = "alpha_scale_stat",
      col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE
    )
  }

  # Return these results ----------------------------------------------------

  return(list(
    psicom = obj,
    keys = keys,
    if (save_item_stat) item_stats <- item_stats,
    if (save_item_stat) scale_stats <- scale_stats,
    alpha = alpha_orig,
    alpha_scale_stat = alpha_orig_scale_stat
  ))
}
