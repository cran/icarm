\name{icarm_pdp}
\alias{icarm_pdp}
\alias{print.icarm_pdp}
\title{Partial Dependence Profile}
\usage{
icarm_pdp(object, data, feature, n_grid = 20L, seed = 2025L)
}
\arguments{
\item{object}{An \code{icarm_model} from \code{\link{icarm_fit}}.}
\item{data}{A data frame used for marginalisation.}
\item{feature}{Character. Name of the feature to profile.}
\item{n_grid}{Integer. Grid resolution for numeric features (default 20).}
\item{seed}{Integer seed (default 2025).}
}
\value{
An \code{icarm_pdp} object with element \code{pdp} (a tibble with columns
\code{feature_value}, \code{mean_pred}, \code{lower}, \code{upper}).
}
\description{
Computes the Partial Dependence Plot (PDP) for a single feature by
marginalising over all other features. At each grid point the feature is
set to a fixed value and predictions are averaged across all observations.
The 10th-90th percentile band shows individual-level variability.
}
\examples{
m   <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
pdp <- icarm_pdp(m, iris, feature = "Petal.Length")
print(pdp)
}
