\name{monitoringStat}
\alias{monitoringStat}
\alias{print.monitoringStat}
\title{
Monitoring Statistic for Stochastic Textured Surfaces
}
\description{
Computes monitoring statistic(s) for local defects (see Bui and Apley 2018a) and/or global changes (see Bui and Apley 2018b) for a given stochastic textured surface image.
}
\usage{
monitoringStat(img, model, type, stat = c("ad", "bp"), w, cl = NULL, verbose = FALSE)
}
\arguments{
  \item{img}{
the given image in the matrix format.
}
  \item{model}{
the object returned by \code{surfacemodel}
}
  \item{type}{
for local defects, \code{type = 1}; for global changes, \code{type = 2}; for both, \code{type = 1:2}
}
  \item{stat}{
for local defects only. The statistic used in the spatial moving statistics. Must be either \code{"ad"} (default) or \code{"bp"}.
}
  \item{w}{
for local defects only. The dimension of the spatial (square) moving window. Must be an odd number >= 3.
}
  \item{cl}{
the object returned by \code{climit} or \code{climit2}.
}
  \item{verbose}{
if set to \code{TRUE}, output monitoring outcome.
}
}
\value{
A \code{monitoringStat} object containing the following components:
\item{sms }{a matrix of the SMS values computed for pixels in \code{img}}
\item{stat }{the \code{stat} argument}
\item{w }{the \code{w} argument}
\item{localStat }{the monitoring statistic for local defects of \code{img}}
\item{globalStat }{the monitoring statistic for global changes of \code{img}}
}

\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, 50, 363–378.
}
\author{
Anh Bui
}
\seealso{
\code{\link{surfacemodel}, \link{sms}, \link{dataPrep}}
}
\examples{
## see the examples in the help file of climit()
?climit
}
