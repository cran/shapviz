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

params <- list(objective = "multi:softprob", num_class = 3)
X_pred <- data.matrix(iris[, -5])
dtrain <- xgb.DMatrix(X_pred, label = as.integer(iris[, 5]) - 1)
fit <- xgb.train(params = params, data = dtrain, nrounds = 50)

## -----------------------------------------------------------------------------
x <- shapviz(fit, X_pred = X_pred, X = iris)
x

# Contains "shapviz" objects for all classes
all.equal(x[[3]], shapviz(fit, X_pred = X_pred, X = iris, which_class = 3))

# Better names
names(x) <- levels(iris$Species)
x

## -----------------------------------------------------------------------------
sv_force(x, row_id = 101)

## -----------------------------------------------------------------------------
sv_importance(x, kind = "bee") +
  plot_layout(ncol = 1)

## -----------------------------------------------------------------------------
names(x) <- levels(iris$Species)

sv_dependence(x, v = "Petal.Length") +
  plot_layout(ncol = 1) &
  xlim(1, 7) &
  ylim(-3, 4)

## -----------------------------------------------------------------------------
X_pred <- data.matrix(iris[, -1])
dtrain <- xgb.DMatrix(X_pred, label = iris[, 1])
fit_xgb <- xgb.train(data = dtrain, nrounds = 50)

## -----------------------------------------------------------------------------
shap_xgb <- shapviz(fit_xgb, X_pred = X_pred, X = iris)
x_subgroups <- split(shap_xgb, f = iris$Species)

## -----------------------------------------------------------------------------
sv_dependence(x_subgroups, v = "Petal.Length") +
  plot_layout(ncol = 1) & 
  xlim(1, 7) &
  ylim(-1.4, 2.2)

