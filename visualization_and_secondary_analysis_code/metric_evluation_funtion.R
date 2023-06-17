# -------------- Loading packages to be used ahead of time
library(dplyr)
library(caret)
library(parallel)
library(doParallel)
library(openxlsx)
library(data.table)
library(rrBLUP)

# Custom formula for calculating metric
R_MAE_RMSE <- function(data, lev = NULL, model = NULL) {
  out <- c(
    cor(data[, "pred"], data[, "obs"], use = "pairwise.complete.obs"),
    cor(data[, "pred"], data[, "obs"], use = "pairwise.complete.obs")^2,
    1 - sum((data[, "obs"] - data[, "pred"])^2) / sum((data[, "obs"] - mean(data[, "obs"]))^2),
    mean((data[, "pred"] - data[, "obs"])^2)^(1 / 2),
    mean(abs(data[, "pred"] - data[, "obs"]))
  )
  names(out) <- c("R", "R2", "Rsquared", "RMSE", "MAE")
  out
}