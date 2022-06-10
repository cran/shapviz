## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 6, 
  fig.height = 4,
  fig.align = "center"
)

library(shapviz)

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(xgboost)

set.seed(3653)

X <- diamonds[c("carat", "cut", "color", "clarity")]
dtrain <- xgb.DMatrix(data.matrix(X), label = diamonds$price)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = dtrain,
  nrounds = 65L
)

## -----------------------------------------------------------------------------
X_small <- X[sample(nrow(X), 2000L), ]

# X is the "explanation" dataset using the original factors
shp <- shapviz(fit, X_pred = data.matrix(X_small), X = X_small)

## ---- dev = 'svg'-------------------------------------------------------------
sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

## ---- fig.asp = .5, dev = 'svg'-----------------------------------------------
sv_force(shp, row_id = 1L)

## -----------------------------------------------------------------------------
# A beeswarm plot
sv_importance(shp)

# Or much simpler: a bar plot of mean absolute SHAP values
sv_importance(shp, kind = "bar")

# Or both!
sv_importance(shp, kind = "both", alpha = 0.2, width = 0.2)

## -----------------------------------------------------------------------------
sv_dependence(shp, v = "color", color_var = "auto")

sv_dependence(shp, v = "carat", color_var = "auto", alpha = 0.2, size = 1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

