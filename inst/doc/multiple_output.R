## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.height = 8, 
  fig.width = 7,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(xgboost)
library(ggplot2)
library(shapviz)
library(patchwork)

params <- list(objective = "multi:softprob", num_class = 3L)
X_pred <- data.matrix(iris[, -5L])
dtrain <- xgb.DMatrix(X_pred, label = as.integer(iris[, 5L]) - 1L)
fit <- xgb.train(params = params, data = dtrain, nrounds = 50L)

## -----------------------------------------------------------------------------
x <- shapviz(fit, X_pred = X_pred, X = iris)
x

# Contains "shapviz" objects for all classes
all.equal(x[[3L]], shapviz(fit, X_pred = X_pred, X = iris, which_class = 3L))

# Better names
names(x) <- levels(iris$Species)
x

## -----------------------------------------------------------------------------
sv_force(x, row_id = 101L)

## -----------------------------------------------------------------------------
sv_importance(x, kind = "bee") +
  plot_layout(ncol = 1L)

## -----------------------------------------------------------------------------
names(x) <- levels(iris$Species)

sv_dependence(x, v = "Petal.Length") +
  plot_layout(ncol = 1L) &
  xlim(1, 7) &
  ylim(-3, 4)

## -----------------------------------------------------------------------------
X_pred <- data.matrix(iris[, -1L])
dtrain <- xgb.DMatrix(X_pred, label = iris[, 1L])
fit_xgb <- xgb.train(data = dtrain, nrounds = 50L)

## -----------------------------------------------------------------------------
shap_xgb <- shapviz(fit_xgb, X_pred = X_pred, X = iris)
x_subgroups <- c(
  setosa     = shap_xgb[iris$Species == "setosa"],
  versicolor = shap_xgb[iris$Species == "versicolor"],
  virginica  = shap_xgb[iris$Species == "virginica"]
)

## -----------------------------------------------------------------------------
sv_dependence(x_subgroups, v = "Petal.Length") +
  plot_layout(ncol = 1L) & 
  xlim(1, 7) &
  ylim(-1.4, 2.2)

