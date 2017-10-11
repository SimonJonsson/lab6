
#' An implementation of a dynamic solution to the knapsack problem
#'
#' @param x data.frame with two vectors of the same size, v with values and w with weights
#' @param W an integer as the total weight of the knapsack
#'
#' @export
knapsack_dynamic <- function(x, W) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  v <- x$v
  w <- x$w
  n <-  length(v)
  m <- matrix(replicate(W * n, 0), nrow = n, ncol = W)
  for (j in 1:W) {
    m[1, j] <- 0
  }

  for (i in 2:n) {
    for (j in 1:W) {
      if (w[i] > j) {
        m[i, j] <- m[i - 1, j]
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - w[i]] + v[i])
      }
    }
  }

  ## No need to read this yet
  ele <- c()
  wt <- W
  i <- n
  while (i > 1) {
    if (m[i, wt] > m[i - 1, wt]) {
      ele <- c(ele, i)
      wt <- wt - w[i]
      i <- i - 1
    } else {
      i <- i - 1
    }

  }

  return(list(value = m[n, W], elements = rev(ele)))
}
