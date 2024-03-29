% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractors.R
\name{extractors}
\alias{extractors}
\alias{get_shap_values}
\alias{get_shap_values.shapviz}
\alias{get_shap_values.mshapviz}
\alias{get_shap_values.default}
\alias{get_feature_values}
\alias{get_feature_values.shapviz}
\alias{get_feature_values.mshapviz}
\alias{get_feature_values.default}
\alias{get_baseline}
\alias{get_baseline.shapviz}
\alias{get_baseline.mshapviz}
\alias{get_baseline.default}
\alias{get_shap_interactions}
\alias{get_shap_interactions.shapviz}
\alias{get_shap_interactions.mshapviz}
\alias{get_shap_interactions.default}
\title{Extractor Functions}
\usage{
get_shap_values(object, ...)

\method{get_shap_values}{shapviz}(object, ...)

\method{get_shap_values}{mshapviz}(object, ...)

\method{get_shap_values}{default}(object, ...)

get_feature_values(object, ...)

\method{get_feature_values}{shapviz}(object, ...)

\method{get_feature_values}{mshapviz}(object, ...)

\method{get_feature_values}{default}(object, ...)

get_baseline(object, ...)

\method{get_baseline}{shapviz}(object, ...)

\method{get_baseline}{mshapviz}(object, ...)

\method{get_baseline}{default}(object, ...)

get_shap_interactions(object, ...)

\method{get_shap_interactions}{shapviz}(object, ...)

\method{get_shap_interactions}{mshapviz}(object, ...)

\method{get_shap_interactions}{default}(object, ...)
}
\arguments{
\item{object}{Object to extract something.}

\item{...}{Currently unused.}
}
\value{
\itemize{
\item \code{get_shap_values()} returns the matrix of SHAP values,
\item \code{get_feature_values()} the \code{data.frame} of feature values,
\item \code{get_baseline()} the numeric baseline value, and
\item \code{get_shap_interactions()} the SHAP interactions of the input.
}

For objects of class "mshapviz", these functions return lists of those elements.
}
\description{
Functions to extract SHAP values, feature values, the baseline,
or SHAP interactions from a "(m)shapviz" object.
}
\examples{
S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
X <- data.frame(x = c("a", "b"), y = c(100, 10))
shp <- shapviz(S, X, baseline = 4)
get_shap_values(shp)
}
