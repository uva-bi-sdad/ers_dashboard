tryTryTry <- function(x, n = 3L) {
  response <- "failed"
  attempt <- 1
  while (response == "failed" && attempt <= n) {
    print(sprintf("attempt: %s", attempt))
    attempt <- attempt + 1
    try({ response <- x }, silent = TRUE)
  }
  response
}
