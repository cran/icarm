\name{icarm_plot_pdp}
\alias{icarm_plot_pdp}
\title{Plot a Partial Dependence Profile}
\usage{
icarm_plot_pdp(pdp, title = NULL)
}
\arguments{
\item{pdp}{An \code{icarm_pdp} from \code{\link{icarm_pdp}}.}
\item{title}{Optional plot title.}
}
\value{A ggplot2 object.}
\description{
Plots the Partial Dependence Profile for one feature.
For numeric features a line with a 10th-90th percentile ribbon is shown;
for categorical features a dot-and-errorbar chart is shown.
}
\examples{
m   <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
pdp <- icarm_pdp(m, iris, feature = "Petal.Length")
p   <- icarm_plot_pdp(pdp)
}
