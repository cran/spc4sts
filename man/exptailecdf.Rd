\name{exptailecdf}
\alias{exptailecdf}
\alias{print.exptailecdf}
\title{
Empirical Cumulative Distribution Function with Exponential Tail Approximation
}
\description{
Computes the empirical cumulative distribution funciton (ecdf) of a given vector of observations, and approximates the tails of the ecdf with exponential curves.
}
\usage{
exptailecdf(x, N = max(2, 0.002 * length(x)), m = min(N, 5))
}
\arguments{
  \item{x}{
the given vector of observations
}
  \item{N}{
the number of observations at each tail of the ecdf used for estimating the exponential curves.
}
  \item{m}{
the \code{m}th observation from each extreme of the ecdf is the starting point to use the estimated exponential curves.
}
}
\details{
An ecdf has a probability of 0 or 1 for any new observation that lies beyond the range of the data of the cedf. This is a problem when using the ecdf as the reference cdf for the one-sample Anderson-Darling (AD) statistic because the computational formula of the AD statistic is infinite with such probabilities. The ecdf with exponential tail approximation replaces the tails of the ecdf with exponential curves, which extend to infinity, to solve this problem. The exponential curves are estimated using the observations at the tails of the ecdf. See Bui and Apley (2018a) for more details.
}
\value{
An object of class \code{exptailecdf}. See \code{\link{exptailecdf.object}}
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{exptailecdf.object}, \link{pexptailecdf}, \link{ecdf}, \link{ad}}
}
\examples{
r <- rnorm(1000)
Fr <- exptailecdf(r)
}
