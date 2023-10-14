## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.height = 5, 
  fig.width = 8,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(xgboost)
library(ggplot2)
library(shapviz)

head(miami)

x_coord <- c("LATITUDE", "LONGITUDE")
x_nongeo <- c("TOT_LVG_AREA", "LND_SQFOOT", "structure_quality", "age")
x <- c(x_coord, x_nongeo)

# Train/valid split
set.seed(1)
ix <- sample(nrow(miami), 0.8 * nrow(miami))
X_train <- data.matrix(miami[ix, x])
X_valid <- data.matrix(miami[-ix, x])
y_train <- log(miami$SALE_PRC[ix])
y_valid <- log(miami$SALE_PRC[-ix])

# Fit XGBoost model with early stopping
dtrain <- xgb.DMatrix(X_train, label = y_train)
dvalid <- xgb.DMatrix(X_valid, label = y_valid)

params <- list(
  learning_rate = 0.2, objective = "reg:squarederror", max_depth = 5, nthread = 1
)

fit <- xgb.train(
  params = params, 
  data = dtrain, 
  watchlist = list(valid = dvalid), 
  early_stopping_rounds = 20,
  nrounds = 1000,
  callbacks = list(cb.print.evaluation(period = 100))
)

## -----------------------------------------------------------------------------
sv <- shapviz(fit, X_pred = X_valid)
sv_dependence(
  sv, 
  v = c("TOT_LVG_AREA", "structure_quality", "LONGITUDE", "LATITUDE"), 
  alpha = 0.2
)

# And now the two-dimensional plot of the sum of SHAP values
sv_dependence2D(sv, x = "LONGITUDE", y = "LATITUDE") +
  coord_equal()

## -----------------------------------------------------------------------------
# Extend the feature set
more_geo <- c("CNTR_DIST", "OCEAN_DIST", "RAIL_DIST", "HWY_DIST")
x2 <- c(x, more_geo)

X_train2 <- data.matrix(miami[ix, x2])
X_valid2 <- data.matrix(miami[-ix, x2])

dtrain2 <- xgb.DMatrix(X_train2, label = y_train)
dvalid2 <- xgb.DMatrix(X_valid2, label = y_valid)

# Build interaction constraint vector
ic <- c(
  list(which(x2 %in% c(x_coord, more_geo)) - 1),
  as.list(which(x2 %in% x_nongeo) - 1)
)

# Modify parameters
params$interaction_constraints <- ic

fit2 <- xgb.train(
  params = params, 
  data = dtrain2, 
  watchlist = list(valid = dvalid2), 
  early_stopping_rounds = 20,
  nrounds = 1000,
  callbacks = list(cb.print.evaluation(period = 100))
)

# SHAP analysis
sv2 <- shapviz(fit2, X_pred = X_valid2)

# Two selected features: Thanks to additivity, structure_quality can be read as 
# Ceteris Paribus
sv_dependence(sv2, v = c("structure_quality", "LONGITUDE"), alpha = 0.2)

# Total geographic effect (Ceteris Paribus thanks to additivity)
sv_dependence2D(sv2, x = "LONGITUDE", y = "LATITUDE", add_vars = more_geo) +
  coord_equal()

