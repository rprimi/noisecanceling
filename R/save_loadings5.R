save_loadings5 <- function(obj,
                           item_dic = NULL,
                           filename,
                           digits = 3,
                           sheetName = "factor_loadings") {
  # library
  library(openxlsx)
  library(dplyr)
  library(psych)
  library(stringr)

  if (class(obj)[2] == "omega") {
    obj$loadings <- obj$schmid$sl
    dimnames(obj$loadings)[[1]] <- str_replace(dimnames(obj$loadings)[[1]], "-", "")
    dimnames(obj$schmid$sl)[[1]] <- str_replace(dimnames(obj$schmid$sl)[[1]], "-", "")
  }


  # non sorted item order
  order <- as.data.frame(
    as.character(dimnames(obj$loadings)[[1]]),
    stringsAsFactors = FALSE
  )
  # sorted item order


  if (class(obj)[2] == "omega") {
    order2 <- as.data.frame(
      as.character(dimnames(fa.sort(obj)$schmid$sl)[[1]]),
      stringsAsFactors = FALSE
    )
  } else {
    order2 <- as.data.frame(
      as.character(dimnames(fa.sort(obj)$loadings)[[1]]),
      stringsAsFactors = FALSE
    )
  }



  # key variable
  names(order)[1] <- "coditem"
  names(order2)[1] <- "coditem"

  # cretae a index to sort in excel
  order2$sort_load <- as.numeric(rownames(order2))

  # combine info in the non sorted order
  order <- left_join(order, order2, by = "coditem")

  # organize results
  if (class(obj)[2] == "omega") {
    results <- data.frame(
      coditem <- rownames(obj$loading),
      round(unclass(obj$loadings), digits = digits),
      stringsAsFactors = FALSE
    )
  } else {
    results <- data.frame(
      coditem <- rownames(obj$loading),
      round(unclass(obj$loadings), digits = digits),
      comp = round(obj$complexity, digits = digits),
      h2 = round(obj$communality, digits = digits),
      stringsAsFactors = FALSE
    )
  }


  names(results)[1] <- "coditem"
  results <- left_join(results, order, by = "coditem")

  if (!is.null(item_dic)) {
    names(item_dic) <- tolower(names(item_dic))
    results <- left_join(results, item_dic, by = "coditem")
  }


  # write results
  openxlsx::write.xlsx(
    x = results,
    file = filename,
    asTable = TRUE
  )

  if (class(obj)[1] == "fa") {
    if (obj$rotation != "varimax") {
      write.xlsx(
        x = obj$Phi, file = filename,
        asTable = TRUE
      )
    }
  }


  return(results)
}
