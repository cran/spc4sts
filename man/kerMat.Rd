\name{kerMat}
\alias{kerMat}
\title{
Epanechnikov quadratic kernel matrix
}
\description{
Computes the Epanechnikov quadratic kernel in 2-D, and returns the positive kernel values.}
\usage{
kerMat(p)
}
\arguments{
  \item{p}{
the bandwidth parameter
}
}
\value{
A matrix containing all the positive kernel values
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{bp}}
}
\examples{
kerMat(5)
}
