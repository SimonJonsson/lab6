## ----setup, include=FALSE------------------------------------------------
library(lab6)
devtools::install_github("hadley/lineprof")
library(lineprof)
set.seed(42)
m <- 2000
knapsack_objects <-
  data.frame(w = sample(1:4000, size = m, replace = TRUE),
             v = runif(n = m, 0, 10000))

## ----brute par-----------------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel=FALSE))

## ----brute non-par-------------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel=FALSE))

## ----dynamic-------------------------------------------------------------
system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

## ----greedy--------------------------------------------------------------
system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

## ----profile setup, include=FALSE----------------------------------------
devtools::install_github("hadley/lineprof")
library(lineprof)

## ----profile brute par---------------------------------------------------
lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel=TRUE))

## ----profile brute non-par-----------------------------------------------
lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel=FALSE))

## ----profile dynamic-----------------------------------------------------
lineprof(knapsack_dynamic(x = knapsack_objects[1:100,], W = 3500))

## ----profile greedy------------------------------------------------------
lineprof(greedy_knapsack(x = knapsack_objects[1:20000,], W = 3500))

