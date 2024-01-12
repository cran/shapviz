## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5.5, 
  fig.height = 3.5,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(xgboost)
library(patchwork)  # We will need its "&" operator

set.seed(1)

# Build model
x <- c("carat", "cut", "color", "clarity")
dtrain <- xgb.DMatrix(data.matrix(diamonds[x]), label = diamonds$price, nthread = 1)
fit <- xgb.train(
  params = list(learning_rate = 0.1, nthread = 1), data = dtrain, nrounds = 65
)

# SHAP analysis: X can even contain factors
dia_2000 <- diamonds[sample(nrow(diamonds), 2000), x]
shp <- shapviz(fit, X_pred = data.matrix(dia_2000), X = dia_2000)

sv_importance(shp, show_numbers = TRUE)
sv_importance(shp, kind = "beeswarm")  # kind = "both" combines bar and bee

## ---- fig.width=8.5, fig.height=5.5-------------------------------------------
sv_dependence(shp, v = x)

## -----------------------------------------------------------------------------
sv_waterfall(shp, row_id = 1) +
  theme(axis.text = element_text(size = 11))

## ---- fig.height=2------------------------------------------------------------
sv_force(shp, row_id = 1)

## -----------------------------------------------------------------------------
sv_waterfall(shp, shp$X$color == "D") +
  theme(axis.text = element_text(size = 11))

## -----------------------------------------------------------------------------
shp_i <- shapviz(
  fit, X_pred = data.matrix(dia_2000[x]), X = dia_2000, interactions = TRUE
)
sv_dependence(shp_i, v = "carat", color_var = x, interactions = TRUE)
sv_interaction(shp_i) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

