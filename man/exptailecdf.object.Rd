\name{exptailecdf.object}
\alias{exptailecdf.object}
\title{
Empirical Cumulative Distribution Function with Exponential Tail Approximation Object
}
\description{
These are objects representing the computed \code{exptailecdf}.
}
\value{
\item{ecdf }{the ecdf return by the \code{stats::ecdf()}}
\item{lambda}{the parameters estimated for the exponential curves. \code{lambda[1]} corresponds to the left tail.}
\item{joint}{where the ecdf started to be replaced by the exponential curves. \code{joint[1]} corresponds to the left tail.}
}
\author{
Anh Bui
}
\seealso{
\code{\link{exptailecdf}}
}
