\name{climit}
\alias{climit}
\alias{climit2}
\alias{print.climit}
\alias{plot.climit}
\title{
Control Limit and Diagnostic Threshold Construction
}
\description{
Establish control limits (for local defects and/or global changes) and diagnostic thresholds (for local defects) from the given Phase I images. \code{climit} is used for the first time. \code{climit2} can update the control limits and diagnostic thresholds given the output of \code{climit}. See Warning. To plot histograms of the Phase I monitoring statistics, use \code{plot.climit}.
}
\usage{
climit(imgs, fa.rate, model, type, stat = c("ad", "bp"), w, nD = 10, verbose = FALSE)
climit2(cl, fa.rate, nD)
}
\arguments{
  \item{imgs}{
a 3-dimensional array containing all Phase I in-control images.
}
  \item{fa.rate}{
the false alarm rate, which asserts the rate of in-control images that are falsely alarmed as out-of-control. This can be a vector, in which case several levels of the control limit are returned.
}
  \item{model}{
the object returned by \code{surfacemodel}.
}
  \item{type}{
for local defects, \code{type = 1}; for global changes, \code{type = 2}; for both, \code{type = 1:2}.
}
  \item{stat}{
for local defects only. The statistic used in the spatial moving statistics. Must be either \code{"ad"} (default) or \code{"bp"}.
}
  \item{w}{
for local defects only. The dimension of the spatial (square) moving window. Must be an odd number >= 3.
}
  \item{nD}{
for local defects only. The parameter to construct the diagnostic threshold. It is the average number of highlighted pixels in the diagnostic image for an in-control image.
}
  \item{verbose}{
if \code{TRUE}, show the computing progress.
}
  \item{cl}{
the object returned by \code{climit} or \code{climit2}.
}
}
\value{
An object of class \code{climit}. See \code{\link{climit.object}}.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, 50, 363-378.
}
\author{
Anh Bui
}
\seealso{
\code{\link{monitoringStat}, \link{diagnoseLD}}
}
\examples{
## build the in-control model
img <- sarGen(m = 50, n = 50, border = 50) # training image
model <- surfacemodel(img, nb = 1, keep.residuals = TRUE)

## after that, generate Phase I images
imgs <- array(0, c(50,50,3))
for (j in 1:dim(imgs)[3])
  imgs[,,j] <- sarGen(phi1 = .6, phi2 = .35, m = 50, n = 50, border = 50)

## establish control limits and diagnostic thresholds
# construct control limits (for both local defects and global changes)
# and diagnostic thresholds (for local defects) for the first time
cl <- climit(imgs, fa.rate = .05, model, type = 1:2, stat = "ad", w = 5, nD = 50)
cl
# update new control limit and diagnostic threshold
cl2 <- climit2(cl, fa.rate = .01, nD = 5)
# plots histograms of Phase I monitoring statistics
plot(cl2)

## after that, monitor a Phase II image as follows:
# create a new image with a local defect
img2 <- sarGen(phi1 = .6, phi2 = .35, m = 50, n = 50, border = 50) # simulate a new image
img3 <- imposeDefect(img2)$img # add a local defect to this image
ms3 <- monitoringStat(img = img3, model = model, cl = cl2) # computing monitoring statistics
# now create a new image with parameters reduced by 5% (representing a global change)
img4 <- sarGen(phi1 = .6*.95, phi2 = .35*.95, m = 50, n = 50, border = 50)
ms4 <- monitoringStat(img = img4, model = model, cl = cl2) # computing monitoring statistics

## diagnose for local defect regions in img3
bimg <- diagnoseLD(ms3, dth = 9, plot.it = FALSE) # use climit() to find dth
par(mfcol = c(1, 2))
par(mar = c(2, 0.5, 1, 0.5))
image(xaxt = 'n', yaxt = 'n', as.matrix(t(apply(img3 , 2, rev))),
      col = gray((0:32)/32), xlab = '', ylab = '', asp = 1, bty = 'n')
image(xaxt = 'n', yaxt = 'n', as.matrix(t(apply(bimg , 2, rev))),
      col = gray(c(1, .5)), xlab = '', ylab = '', asp = 1, bty = 'n')

#
# NOTE: The above example is just for quick illustration. To obtain a good
# control limit, the training image should be representative (e.g., set
# m = 250, n = 250, and border = 200). The number of Phase I images also
# needs to be large (e.g., 100 images or more).
#
# For real images in a textile application, use the R data package "textile".
#

}
