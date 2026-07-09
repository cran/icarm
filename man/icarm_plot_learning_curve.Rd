\name{icarm_plot_learning_curve}
\alias{icarm_plot_learning_curve}
\title{Plot a Learning Curve}
\usage{
icarm_plot_learning_curve(lc, title = NULL)
}
\arguments{
\item{lc}{An \code{icarm_learning_curve} from \code{\link{icarm_learning_curve}}.}
\item{title}{Optional plot title.}
}
\value{A ggplot2 object.}
\description{
Plots training and validation error against training set size, with
+/- 1 SD bands. A large gap indicates high variance (overfitting);
both curves high and close together indicates high bias (underfitting).
}
\examples{
lc <- icarm_learning_curve(Species ~ ., iris, model = "cart",
                            cv_folds = 3L,
                            sizes = seq(0.2, 0.8, by = 0.2))
p  <- icarm_plot_learning_curve(lc)
}
