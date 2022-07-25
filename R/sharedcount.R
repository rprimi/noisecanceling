sharedcount <- function(x, ...) {
  nx <- names(x)
  alln <- combn(nx, 2)
  out <- apply(
    alln, 2,
    function(y) sum(complete.cases(x[y]))
  )
  data.frame(t(alln), out)
}
