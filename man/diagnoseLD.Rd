\name{diagnoseLD}
\alias{diagnoseLD}
\title{
Diagnose Local Defects on Stochastic Textured Surfaces
}
\description{
Produces a binary diagnostic image of a given stochastic textured surface image based on its spatial moving statistics.
}
\usage{
diagnoseLD(ms, dth, plot.it = TRUE)
}
\arguments{
  \item{ms}{
the object return by \code{monitoringStat()}
}
  \item{dth}{
the diagnostic threshold
}
  \item{plot.it}{
plots the binary diagnositc image if set to \code{TRUE}
}
}
\value{
The binary diagnostic image in the matrix format.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{monitoringStat}, \link{climit}}
}
\examples{
## ## see the examples in the help file of climit()
?climit
}
