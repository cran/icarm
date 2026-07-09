\name{icarm_shap}
\alias{icarm_shap}
\alias{print.icarm_shap}
\title{SHAP-style local feature attributions}
\usage{
icarm_shap(object, data, n_samples = 50L, max_obs = 100L, seed = 2025L)
}
\arguments{
\item{object}{An \code{icarm_model} from \code{\link{icarm_fit}}.}
\item{data}{A data frame of observations to explain.}
\item{n_samples}{Integer. Background sample size (default 50).}
\item{max_obs}{Integer. Maximum observations to explain (default 100).}
\item{seed}{Integer seed for reproducibility (default 2025).}
}
\value{
An \code{icarm_shap} object with elements \code{shap} (long-format tibble
of SHAP values), \code{importance} (features ranked by mean |SHAP|), and
\code{baseline} (mean prediction over the background sample).
}
\description{
Computes approximate SHAP (SHapley Additive exPlanations) values using a
marginal interventional approach. For each observation and each feature
the SHAP value is the average change in prediction when that feature is set
to the observed value versus its background distribution.
Works with any \code{icarm_model} without additional packages.
}
\examples{
m    <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
shap <- icarm_shap(m, iris[1:20, ], n_samples = 20L)
print(shap)
}
