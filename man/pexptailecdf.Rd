\name{pexptailecdf}
\alias{pexptailecdf}
\title{
Predictions from an Exptailecdf Object
}
\description{
Returns the values of the \code{exptailecdf} object at given observations.
}
\usage{
pexptailecdf(Fx, y)
}
\arguments{
  \item{Fx}{
the object of class \code{exptailecdf}, containing an ecdf with exponential tail approximation.
}
  \item{y}{
the given observations in the scalar/vector/matrix format.
}
}
\value{
An object of the same type with \code{y} that stores the evaluations of the \code{exptailecdf} object at the given \code{y}.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{exptailecdf.object}, \link{exptailecdf}}
}
\examples{
r <- rnorm(1000)
Fr <- exptailecdf(r)

pexptailecdf(Fr, max(r) + .1)
pexptailecdf(Fr, c(min(r) - .1, max(r) + .1))
pexptailecdf(Fr, matrix(c(.8, .9, 1, 1.1), 2, 2))
}
