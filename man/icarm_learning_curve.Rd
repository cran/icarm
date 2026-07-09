\name{icarm_learning_curve}
\alias{icarm_learning_curve}
\alias{print.icarm_learning_curve}
\title{Learning Curve Diagnostics}
\usage{
icarm_learning_curve(
  formula,
  data,
  model = "auto",
  sizes = seq(0.1, 0.9, by = 0.1),
  cv_folds = 5L,
  seed = 2025L,
  ...
)
}
\arguments{
\item{formula}{A model formula.}
\item{data}{A data frame.}
\item{model}{Character model type (default \code{"auto"}). See \code{\link{icarm_fit}}.}
\item{sizes}{Numeric vector of training-set fractions (default 0.1 to 0.9).}
\item{cv_folds}{Integer. Repetitions per size (default 5).}
\item{seed}{Integer seed (default 2025).}
\item{...}{Additional arguments forwarded to \code{\link{icarm_fit}}.}
}
\value{
An \code{icarm_learning_curve} object with elements \code{curve} (tibble
of training and validation errors at each size), \code{metric}, and
\code{task}.
}
\description{
Trains models on progressively larger subsets of the data and records
training and validation error at each size, repeated over \code{cv_folds}
random splits. Use the resulting curve to diagnose high bias (both errors
high and close) versus high variance (large gap between train and
validation errors).
}
\examples{
lc <- icarm_learning_curve(Species ~ ., iris,
                            model = "cart", cv_folds = 3L,
                            sizes = seq(0.2, 0.8, by = 0.2))
print(lc)
}
