#' SHAP Waterfall Plot
#'
#' Creates a waterfall plot of SHAP values of one single observation. The value of
#' f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the baseline
#' SHAP value. The plot has to be read from bottom to top.
#'
#' @param object An object of class "shapviz".
#' @param row_id A single row number to plot.
#' @param max_display Maximum number of features (with largest absolute SHAP values)
#' should be plotted? If there are more features, they will be collapsed to one feature.
#' The default is ten in order to not overload the plot. Set to \code{Inf} to show
#' all features.
#' @param sort_fun Function used to sort the SHAP values for plotting.
#' By default, the values are sorted by absolute SHAP values, i.e., the default
#' is \code{function(shap) = abs(shap)}. The result of the function is passed to
#' \code{order()}.
#' @param fill_colors A vector of exactly two fill colors: the first for positive
#' SHAP values, the other for negative ones.
#' @param format_fun Function used to (A) format numeric feature values shown on the
#' axis labels, and (B) to format SHAP values shown in the arrow bars.
#' @param contrast Logical flag that detemines whether to use white text in dark arrows.
#' Default is \code{TRUE}.
#' @param show_connection Should connecting lines be shown? Default is \code{TRUE}.
#' @param show_annotation Should "f(x)" and "E(f(x))" be plotted? Default is \code{TRUE}.
#' @param annotation_size Size of the annotation text (f(x)=... and E(f(x))=...).
#' @param ... Arguments passed to \code{ggfittext::geom_fit_text()}.
#' For example, \code{size = 9} will use fixed text size in the bars and \code{size = 0}
#' will altogether suppress adding text to the bars.
#' @return An object of class \code{ggplot} representing a waterfall plot.
#' @export
#' @seealso \code{\link{sv_force}}.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
#' sv_waterfall(x)
#' sv_waterfall(x, row_id = 123, max_display = 2, size = 9, fill_colors = 4:5)
#'
#' X <- as.data.frame(matrix(1:100, nrow = 10))
#' S <- as.matrix(X)
#' shp <- shapviz(S, X)
#' sv_waterfall(shp)
sv_waterfall <- function(object, ...) {
  UseMethod("sv_waterfall")
}

#' @describeIn sv_waterfall Default method.
#' @export
sv_waterfall.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_waterfall SHAP waterfall plot for an object of class "shapviz".
#' @export
sv_waterfall.shapviz <- function(object, row_id = 1L, max_display = 10L,
                                 sort_fun = function(shap) abs(shap),
                                 fill_colors = c("#f7d13d", "#a52c60"),
                                 format_fun = function(z)
                                   prettyNum(z, digits = 3, scientific = FALSE),
                                 contrast = TRUE, show_connection = TRUE,
                                 show_annotation = TRUE, annotation_size = 3.2, ...) {
  stopifnot(
    "Only one row number can be passed" = length(row_id) == 1L,
    "Exactly two fill colors must be passed" = length(fill_colors) == 2L,
    "Not a function" = is.function(format_fun),
    "Not a function" = is.function(sort_fun)
  )
  X <- get_feature_values(object)[row_id, ]
  S <- get_shap_values(object)[row_id, ]
  b <- get_baseline(object)
  dat <- data.frame(S = S, label = paste(names(X), format_fun(X), sep = " = "))

  # Collapse unimportant features
  dat <- .collapse(dat, S, max_display = max_display)
  m <- nrow(dat)

  # Add order dependent columns
  dat <- dat[order(sort_fun(dat$S)), ]
  dat$i <- seq_len(m)
  dat$to <- cumsum(dat$S) + b
  dat$from <- .lag(dat$to, default = b)

  # Make a waterfall plot
  height <- grid::unit(1 / (1 + 2 * m), "npc")

  p <- ggplot(
    dat,
    aes(
      xmin = from,
      xmax = to,
      y = stats::reorder(label, i),
      fill = factor(to < from, levels = c(FALSE, TRUE))
    )
  ) +
    gggenes::geom_gene_arrow(
      show.legend = FALSE,
      arrowhead_width = grid::unit(2, "mm"),
      arrowhead_height = height,
      arrow_body_height = height
    ) +
    ggfittext::geom_fit_text(
      aes(label = paste0(ifelse(S > 0, "+", ""), format_fun(S))),
      show.legend = FALSE,
      contrast = contrast,
      ...
    ) +
    scale_fill_manual(values = fill_colors, drop = FALSE) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.y = element_blank()
    ) +
    labs(y = element_blank(), x = "SHAP value")

  if (show_connection) {
    p <- p +
      geom_segment(
        aes(x = to, xend = to, y = i, yend = .lag(i, lead = TRUE, default = m)),
        size = 0.3,
        linetype = 2
      )
  }

  if (show_annotation) {
    full_range <- c(dat[m, "to"], dat[1L, "from"])
    p <- p +
      annotate(
        "segment",
        x = full_range,
        xend = full_range,
        y = c(m, 1),
        yend = c(m, 1) + m * c(0.075, -0.075) + 0.13 * c(1, -1),
        size = 0.3,
        linetype = 2
      ) +
      annotate(
        "text",
        x = full_range,
        y = c(m, 1) + m * c(0.1, -0.1) + 0.15 * c(1, -1),
        label = paste0(c("f(x)=", "E[f(x)]="), format_fun(full_range)),
        size = annotation_size
      ) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      scale_y_discrete(expand = expansion(add = 0.3, mult = 0.2)) +
      coord_cartesian(clip = "off")
  }
  p
}

.lag <- function(z, default = NA, lead = FALSE) {
  n <- length(z)
  if (n < 2L) {
    return(rep(default, times = n))
  }
  if (isTRUE(lead)) {
    return(c(z[2L:n], default))
  }
  c(default, z[1L:(n - 1L)])
}

.collapse <- function(dat, S, max_display) {
  ok <- utils::head(names(sort(abs(S), decreasing = TRUE)), max_display - 1L)
  if (length(ok) < nrow(dat) - 1L) {
    bad <- setdiff(names(S), ok)
    dat <- rbind(
      dat[ok, ],
      data.frame(
        S = sum(dat[bad, "S"]),
        label = paste(length(bad), "other features"),
        row.names = "other"
      )
    )
  }
  dat
}
