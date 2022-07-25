map_factor_levels <- function(x) {
  levels <- as.numeric(1:length(levels(x)))
  labels <- levels(x)
  out <- as.data.frame(cbind(levels, labels), stringsAsFactors = FALSE)
  return(out)
}
