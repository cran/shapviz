#' Interaction Strength
#'
#' Returns a vector of interaction strengths between variable `v` and all other
#' variables, see Details.
#'
#' If SHAP interaction values are available, the interaction strength
#' between feature `v` and another feature `v'` is measured by twice their
#' mean absolute SHAP interaction values.
#'
#' Otherwise, we use a heuristic calculated as follows:
#' 1. If `v` is numeric, it is binned into `nbins` bins.
#' 2. Per bin, the SHAP values of `v` are regressed onto `v`, and the R-squared
#'   is calculated. Rows with missing `v'` are discarded.
#' 3. The R-squared are averaged over bins, weighted by the number of
#'   non-missing `v'` values.
#'
#' This measures how much variability in the SHAP values of `v` is explained by `v'`,
#' after accounting for `v`.
#'
#' Set `scale = TRUE` to multiply the R-squared by the within-bin variance
#' of the SHAP values. This will put higher weight to bins with larger scatter.
#'
#' Set `color_num = FALSE` to *not* turn the values of the "color" feature `v'`
#' to numeric.
#'
#' Finally, set `adjusted = TRUE` to use *adjusted* R-squared.
#'
#' The algorithm does not consider observations with missing `v'` values.
#'
#' @param obj An object of class "shapviz".
#' @param v Variable name to calculate potential SHAP interactions for.
#' @param nbins Into how many quantile bins should a numeric `v` be binned?
#'   The default `NULL` equals the smaller of \eqn{n/20} and \eqn{\sqrt n} (rounded up),
#'   where \eqn{n} is the sample size. Ignored if `obj` contains SHAP interactions.
#' @param color_num Should other ("color") features `v'` be converted to numeric,
#'   even if they are factors/characters? Default is `TRUE`.
#'   Ignored if `obj` contains SHAP interactions.
#' @param scale Should adjusted R-squared be multiplied with the sample variance of
#'   within-bin SHAP values? If `TRUE`, bins with stronger vertical scatter will get
#'   higher weight. The default is `FALSE`. Ignored if `obj` contains SHAP interactions.
#' @param adjusted Should *adjusted* R-squared be used? Default is `FALSE`.
#' @returns A named vector of decreasing interaction strengths.
#' @export
#' @seealso [sv_dependence()]
potential_interactions <- function(
    obj, v, nbins = NULL, color_num = TRUE, scale = FALSE, adjusted = FALSE
  ) {
  stopifnot(is.shapviz(obj))
  S <- get_shap_values(obj)
  S_inter <- get_shap_interactions(obj)
  X <- get_feature_values(obj)
  nms <- colnames(obj)
  v_other <- setdiff(nms, v)
  stopifnot(v %in% nms)
  if (ncol(obj) <= 1L) {
    return(NULL)
  }

  # Simple case: we have SHAP interaction values
  if (!is.null(S_inter)) {
    return(sort(2 * colMeans(abs(S_inter[, v, ]))[v_other], decreasing = TRUE))
  }

  # Complicated case: calculate heuristic per color variable
  if (is.null(nbins)) {
    nbins <- ceiling(min(sqrt(nrow(X)), nrow(X) / 20))
  }
  out <- vapply(
    X[v_other],
    FUN = heuristic,
    FUN.VALUE = 1.0,
    s = S[, v],
    bins = .fast_bin(X[[v]], nbins = nbins),
    color_num = color_num,
    scale = scale,
    adjusted = adjusted
  )
  sort(out, decreasing = TRUE, na.last = TRUE)
}

#' Interaction Heuristic
#'
#' Internal function used to calculate the interaction heuristics described in
#' [potential_interactions()]. It calls `heuristic_in_bin()` per bin and aggregates
#' the result.
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams potential_interactions
#' @param color Feature values of the "color" feature.
#' @param s SHAP values of `v`.
#' @returns A single number.
heuristic <- function(color, s, bins, color_num, scale, adjusted) {
  if (isTRUE(color_num)) {
    color <- .as_numeric(color)
  }
  M <- mapply(
    heuristic_in_bin,
    color = split(color, bins),
    s = split(s, bins),
    MoreArgs = list(scale = scale, adjusted = adjusted)
  )
  stats::weighted.mean(M[1L, ], M[2L, ], na.rm = TRUE)
}

#' Interaction Heuristic in Bin
#'
#' Internal function used to calculate the within-bin heuristic used in `heuristic()`.
#' See `heuristic()` for details.
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams heuristic
#' @returns
#'   A (1x2) matrix with the heuristic and the number of observations with non-missing
#'   `v'`.
heuristic_in_bin <- function(color, s, scale = FALSE, adjusted = FALSE) {
  ok <- !is.na(color)
  color <- color[ok]
  s <- s[ok]
  n <- length(s)
  var_s <- stats::var(s)
  if (n < 2L || var_s < .Machine$double.eps || length(unique(color)) < 2L) {
    return(cbind(stat = 0, n = n))
  }
  z <- stats::lm(s ~ color)
  var_r <- sum(z$residuals^2) / (if (adjusted) z$df.residual else n - 1)
  stat <- 1 - var_r / var_s
  if (scale) {
    stat <- stat * var_s
  }
  if (!is.finite(stat)) {
    stat <- 0
  }
  cbind(stat = stat, n = n)
}

# Like as.numeric(), but can deal with factor variables
.as_numeric <- function(z) {
  if (is.numeric(z)) {
    return(z)
  }
  if (is.character(z)) {
    z <- factor(z)
  }
  as.numeric(z)
}

# Bins discrete z into integer valued bins
.fast_bin <- function(z, nbins) {
  if (.is_discrete(z, n_unique = nbins)) {
    return(z)
  }
  q <- stats::quantile(z, seq(0, 1, length.out = nbins + 1L), na.rm = TRUE)
  findInterval(z, unique(q), rightmost.closed = TRUE)
}
