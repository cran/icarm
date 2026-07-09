\name{icarm_plot_shap}
\alias{icarm_plot_shap}
\title{Beeswarm SHAP plot}
\usage{
icarm_plot_shap(shap, n_features = 10L, title = NULL)
}
\arguments{
\item{shap}{An \code{icarm_shap} from \code{\link{icarm_shap}}.}
\item{n_features}{Max features to show (default 10).}
\item{title}{Optional plot title.}
}
\value{A ggplot2 object.}
\description{
Displays SHAP values as a beeswarm-style scatter. Each point is one
observation; the x-axis shows the SHAP value; colour encodes the
feature value (blue = low, red = high).
}
\examples{
m    <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
shap <- icarm_shap(m, iris[1:30, ], n_samples = 20L)
p    <- icarm_plot_shap(shap)
}
