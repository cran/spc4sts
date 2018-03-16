\name{ad}
\alias{ad}
\title{
One-Sample Anderson-Darling Statistic
}
\description{
Computes the one-sample Anderson-Darling (AD) statistic.
}
\usage{
ad(r, P)
}
\arguments{
  \item{r}{
the given vector/matrix of observations
}
  \item{P}{
the vector/matrix containing the values of a (reference) cumulative distribution function evaluated at the values in \code{r}.
}
}
\value{
The AD statistic.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{exptailecdf}, \link{sms}, \link{bp}}
}
\examples{
img <- matrix(rnorm(100), 10, 10)
ad(img, pnorm(img))
}
