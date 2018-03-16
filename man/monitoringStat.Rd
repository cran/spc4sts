\name{monitoringStat}
\alias{monitoringStat}
\title{
Monitoring Statistic for Stochastic Textured Surfaces
}
\description{
Computes monitoring statistic(s) for local defects (see Bui and Apley 2017a) and/or global changes (see Bui and Apley 2017b) for a given stochastic textured surface image.
}
\usage{
monitoringStat(img, model, type, stat = c("ad", "bp"),
               w, xval = 1, cl = NULL, verbose = FALSE)
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
  \item{xval}{
for global changes only. The number of folds in cross-validation. Recommended value: 1, i.e., do not use cross-validation to find the complexity parameters for the models reprenting Phase I images. In this case, the complexity parameter in the element \code{fit} of the argument \code{model} is used. This usually results in fast and quite accurate monitoring performance.
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
\item{xval }{the \code{xval} argument}
\item{globalStat }{the monitoring statistic for global changes of \code{img}}
}

\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, to appear.
}
\author{
Anh Bui
}
\seealso{
\code{\link{surfacemodel}, \link{sms}, \link{dataPrep}}
}
\examples{
## build the in-control model
img <- sarGen(phi1 = .6, phi2 = .35, m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img,1)

## generate an image and compute its monitoring statistic
img2 <- sarGen(phi1 = .6, phi2 = .35, m = 100, n = 100, border = 50)
ms <- monitoringStat(img2, model, type = 1:2, stat = "ad", w = 5, xval = 1)
ms$localStat # monitoring statistic for local defects
ms$globalStat # monitoring statistic for global changes
}
