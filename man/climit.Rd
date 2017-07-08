\name{climit}
\alias{climit}
\alias{climit2}
\title{
Control Limit and Diagnostic Threshold Construction
}
\description{
Establish control limits and diagnostic thresholds from given Phase I in-control images. \code{climit} is used for the first time. \code{climit2} can recompute the control limits and diagnostic thresholds given the output of \code{climit}. See Warning.
}
\usage{
climit(imgs, model, stat, w, grate = c(0.05, 0.0027), nD = seq(5, 100, length.out = 10))
climit2(object, grate, nD)
}
\arguments{
  \item{imgs}{
an array containing all Phase I in-control images, each of which is a matrix.
}
  \item{model}{
the output of \code{surfacemodel}
}
  \item{stat}{
the statistic used in the SMS. Can be either "ad" or "bp" (default).
}
  \item{w}{
the window size of the SMS. Must be an odd number.
}
  \item{grate}{
the global false alarm rate of the control limit to be constructed.
}
  \item{nD}{
the parameter to construct the diagnostic threshold. It is the average number of pixels in each diagnostic image that are beyond the control llmit.
}
  \item{object}{
the output of \code{climit}.
}
}
\value{
A list of the following:
\item{nDmaxSms }{a vector that stores the (nD*N + 1) largest SMS values computed for all N Phase I images.}
\item{PIstats }{a vector that stores all the monitoring statistics of the Phase images.}
\item{diagnostic.threshold }{a scalar/vector that stores the established diagnostic threshold(s).}
\item{stat }{the argument \code{stat}}
\item{w }{the argument \code{w}}
\item{control.limit }{a scalar/vector that stores the established control limit(s).}
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\section{Warning }{It is better to set the maximum value of \code{nD} large enough. \code{climit} will only keep the information (for \code{climit2}) to establish the diagnostic threshold up to this value.}
\seealso{
\code{\link{localStat}, \link{diagnoseLD}}
}
\examples{
## build the in-control model
img <- sarGen(m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img,1)

## generate Phase I images
imgs <- array(0, c(100,100,2))
for (j in 1:dim(imgs)[3]) imgs[,,j] <- sarGen(m = 100, n = 100, border = 50)

## establish control limits and diagnostic thresholds
# construct control limits and diagnostic thresholds for the first time
cl <- climit(imgs, model, "bp", 5, .05, 50)
# update new control limit and diagnostic threshold
cl2 <- climit2(cl, .01, 5)

#
# NOTE: The above example is just for quick illustration. To obtain a good
# control limit, the training image should be representative (e.g., set
# m = 250, n = 250, and border = 200). The number of Phase I images also
# needs to be large (e.g., 100 images or more).
#
# For real images in a textile application, use the data package "textile".
#

}
