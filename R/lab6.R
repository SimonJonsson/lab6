#' @title lab6
#'
#' @description Three different implementations of solving the knapsack problem
#' Dynamic, brute force and greedy
#'
#' @name lab6
#' @docType package
NULL

library(lineprof)
set.seed(42)
m <- 2000
knapsack_objects <-
  data.frame(w = sample(1:4000, size = m, replace = TRUE),
             v = runif(n = m, 0, 10000))

#library(parallel)
#' An implementation of the brute force solution to the knapsack problem
#' Where we list all possible combinations of a vector with size \{0,1\}\*n and then make it into
#' a matrix and check each row for best possible value. The matrix has the size 2^n rows and n columns.
#'
#' @param x data.frame with two vectors of the same size, v with values and w with weights
#' @param W an integer as the total weight of the knapsack
#'
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  n <- length(x[[1]])
  l <- rep(list(0:1), n)
  # generates all permutations(2^n) of the n length 0,1 vector
  M <- matrix(unlist(expand.grid(l)), ncol = n)
  v <- x$v
  w <- x$w
  best <- replicate(n, 0)

  if (parallel) {
    # Checks each row of M if the weight is allowed
    res_vec <- mclapply(1:2 ^ n, function(x, M, w, v, W) {
      m <- M[x, ]
      if (sum(w[m == 1]) <= W) {
        return(m)
      }
    }, M, w, v, W, mc.cores = 4)

    # Takes all the allowed rows and calculates which one has the best value
    lapply(Filter(Negate(is.null), res_vec), function(m) {
      if (sum(v[m == 1]) > sum(v[best == 1])) {
        best <<- m
      }
    })
  } else {
    # Goes through each row of M to find the optimal value
    apply(M, 1, function(m) {
      if (sum(v[m == 1]) > sum(v[best == 1]) & sum(w[m == 1]) <= W) {
        best <<- m
      }

    })
  }

  res <- list(value = sum(v[best == 1]), elements = which(best == 1))
  return(res)
}
lineprof(greedy_knapsack(x = knapsack_objects[1:1200, ], W = 2000))

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

lineprof(knapsack_dynamic(x = knapsack_objects[1:12,], W=3500))

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


