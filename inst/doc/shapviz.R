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

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(xgboost)

set.seed(3653)

# Turn ordinal factors into normal ones
ord <- c("clarity", "cut", "color")
diamonds[, ord] <- lapply(diamonds[, ord], factor, ordered = FALSE)

# Fit XGBoost model
x <- c("carat", "clarity", "cut", "color")
dtrain <- xgb.DMatrix(data.matrix(diamonds[x]), label = diamonds$price)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = dtrain,
  nrounds = 65L
)

## -----------------------------------------------------------------------------
# Pick explanation data
dia_small <- diamonds[sample(nrow(diamonds), 2000L), ]

# We also pass feature data X with originally encoded values
shp <- shapviz(fit, X_pred = data.matrix(dia_small[x]), X = dia_small)

## ---- dev = 'svg'-------------------------------------------------------------
sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

## ---- fig.asp = .5, dev = 'svg'-----------------------------------------------
sv_force(shp, row_id = 1L)

## -----------------------------------------------------------------------------
# A bar plot of mean absolute SHAP values
sv_importance(shp)

# A beeswarm plot
sv_importance(shp, kind = "beeswarm")

# Or both!
sv_importance(shp, kind = "both", show_numbers = TRUE, bee_width = 0.2)

## -----------------------------------------------------------------------------
sv_dependence(shp, v = "color", color_var = "auto")

sv_dependence(shp, v = "carat", color_var = "auto", alpha = 0.2, size = 1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

