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
climit(imgs, model, stat = c("ad", "bp"), w, grate = c(0.05, 0.0027), nD = c(5, 100))
climit2(cl, grate, nD)
}
\arguments{
  \item{imgs}{
a 3-dimensional array containing all Phase I in-control images.
}
  \item{model}{
the returned object from \code{surfacemodel}
}
  \item{stat}{
the statistic used in the SMS. Must be either \code{"ad"} (default) or \code{"bp"}.
}
  \item{w}{
the window size of the SMS. Must be an odd number >= 3.
}
  \item{grate}{
the global false alarm rate, which asserts the rate of in-control images that are falsely alarmed as out-of-control. This can be a vector, in which case several levels of the control limit are returned.
}
  \item{nD}{
the parameter to construct the diagnostic threshold. It is the average number of pixels in each diagnostic image that are beyond the control limit.
}
  \item{cl}{
the returned object from \code{climit}.
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
imgs <- array(0, c(100,100,1))
for (j in 1:dim(imgs)[3]) imgs[,,j] <- sarGen(m = 100, n = 100, border = 50)

## establish control limits and diagnostic thresholds
# construct control limits and diagnostic thresholds for the first time
cl <- climit(imgs, model, stat = "ad", w = 5, grate = .05, nD = 50)
# update new control limit and diagnostic threshold
cl2 <- climit2(cl, grate = .01, nD = 5)

#
# NOTE: The above example is just for quick illustration. To obtain a good
# control limit, the training image should be representative (e.g., set
# m = 250, n = 250, and border = 200). The number of Phase I images also
# needs to be large (e.g., 100 images or more).
#
# For real images in a textile application, use the data package "textile".
#

}
