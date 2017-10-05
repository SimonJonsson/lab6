set.seed(42)
m <- 2000
knapsack_objects <-
  data.frame(w = sample(1:4000, size = m, replace = TRUE),
             v = runif(n = m, 0, 10000))

library(parallel)
#' An implementation of the brute force solution to the knapsack problem
#' Where we list all possible combinations of a vector with size \{0,1\}\*n and then make it into
#' a matrix and check each row for best possible value. The matrix has the size 2^n rows and n columns.
#'
#' @param x data.frame with two vectors of the same size, v with values and w with weights
#' @param W an integer as the total weight of the knapsack
#'
#' @export
brute_force_knapsack <- function(x,W, parallel = FALSE) {
  n <- length(x[[1]])
  l <- rep(list(0:1), n)
  M <- matrix(unlist(expand.grid(l)), ncol = n)
  v <- x$v
  w <- x$w
  best <- replicate(n,0)

  # Checks each row of M if the weight is allowed
  if(parallel){
    res_vec <- mclapply(1:2^n, function(x, best, M, w, v, W) {
      m <- M[x,]
      if(sum(w[m == 1]) <= W) {
        return(m)
      }
    }, best, M, w, v, W, mc.cores = detectCores())
    # To reduce overhead - might just use max() and return pairs ^ (m, v[m])
    res_vec <- Filter(Negate(is.null), res_vec)
    lapply(res_vec, function(m) {
      if (sum(v[m == 1]) >= sum(v[best == 1])) {
        best <<- m
      }
    })
  } else {
    apply(M, 1, function(m) {
      if (sum(v[m == 1]) >= sum(v[best == 1]) & sum(w[m == 1]) <= W) {
        best <<- m
      }

    })
  }

  res <- list(value=sum(v[best == 1]), elements=which(best == 1), resVec=best)
  return(res)
}

ptm <- proc.time()
brute_force_knapsack(x = knapsack_objects[1:25, ], W = 3500, parallel = TRUE)
proc.time() - ptm


ptm <- proc.time()
brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel = FALSE)
proc.time() - ptm

# We should return the path
knapsack_dynamic <- function(x,W) {
  n <- length(x[[1]])
  m <- matrix(replicate(W*n,0), nrow=n, ncol=W)
  v <- x$v
  w <- x$w
  for (j in 1:W) {
    m[1,j] <- 0
  }

  for (i in 2:n) {
    for (j in 1:W) {
      if (w[i] > j) {
        m[i,j] <- m[i-1,j]
      } else {
        m[i,j] <- max(m[i-1,j], m[i-1,j-w[i]] + v[i])
      }
    }
  }

  return(m[1:n,W])
}

knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

greedy_knapsack <- function(x,W) {
  n <- length(x[[1]])
  v <- x$v
  w <- x$w
  best <- replicate(n,0)

  # Sort vector o by v_i/w_i
  ind_o <- order(v / w,decreasing=TRUE)

  # Fill the return vector with as many o_1, o_2,..., o_n as possible
  lapply(ind_o, function(i) {
    if(sum(w[best > 0]) <= W){
      best[i] <<- floor((W - w[i] - sum(w[best > 0])) / w[i])
    }
  })
  res <- list(value=sum(v[best > 0]), elements=which(best > 0), bestval=best[best > 0])
  return(res)
}

greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
