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

# Turn ordinal factors into normal ones (required for some of the examples)
ord <- c("clarity", "cut", "color")
diamonds[, ord] <- lapply(diamonds[, ord], factor, ordered = FALSE)

# Fit XGBoost model
x <- c("carat", "clarity", "cut", "color")
dtrain <- xgb.DMatrix(data.matrix(diamonds[x]), label = diamonds$price)
fit <- xgb.train(params = list(learning_rate = 0.1), data = dtrain, nrounds = 65L)

## -----------------------------------------------------------------------------
# Explanation data
dia_small <- diamonds[sample(nrow(diamonds), 2000L), ]

shp <- shapviz(fit, X_pred = data.matrix(dia_small[x]), X = dia_small)

## -----------------------------------------------------------------------------
sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

## ---- fig.asp = .5------------------------------------------------------------
sv_force(shp, row_id = 1L)

## -----------------------------------------------------------------------------
sv_waterfall(shp, shp$X$color == "D") +
  theme(axis.text = element_text(size = 11))

## -----------------------------------------------------------------------------
# A barplot of mean absolute SHAP values
sv_importance(shp)

# A beeswarm plot
sv_importance(shp, kind = "beeswarm")

# Or both!
sv_importance(shp, kind = "both", show_numbers = TRUE, bee_width = 0.2)

## -----------------------------------------------------------------------------
sv_dependence(shp, v = "color")

sv_dependence(shp, v = "carat", alpha = 0.2, size = 1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

## ---- fig.height=5.5, fig.width=7---------------------------------------------
library(patchwork)  # to use the & operator

sv_dependence(shp, v = x) &
  theme_gray(base_size = 9) &
  ylim(-5000, 15000)

## -----------------------------------------------------------------------------
shp_i <- shapviz(
  fit, X_pred = data.matrix(dia_small[x]), X = dia_small, interactions = TRUE
)

sv_dependence(shp_i, v = "color", color_var = "cut", interactions = TRUE)

## -----------------------------------------------------------------------------
sv_dependence(shp_i, v = "carat", color_var = x, interactions = TRUE) &
  ylim(-6000, 13000)

## -----------------------------------------------------------------------------
sv_interaction(shp_i) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

