
#' An implementation of a greedy solution to the knapsack problem
#'
#' @param x data.frame with two vectors of the same size, v with values and w with weights
#' @param W an integer as the total weight of the knapsack
#'
#' @export
greedy_knapsack <- function(x, W) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  v <- x$v
  w <- x$w
  n <-  length(v)
  best <- replicate(n, 0)

  # Sort vector o by v_i/w_i
  ind_o <- order(v / w, decreasing = TRUE)

  # Fill the return vector with as many o_1, o_2,..., o_n as possible
  for(i in ind_o) {
    # How many values we can squeeze into the knapsack
    squeeze <- (W - w[i] - sum(w[best > 0])) / w[i]
    if (squeeze > 0) {
      best[i] <- squeeze
    } else {
      break
    }
  }

  res <- list(value = sum(v[best > 0]), elements = which(best > 0))
  return(res)
}
