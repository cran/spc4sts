\name{dataPrep}
\alias{dataPrep}
\title{
Neighborhood Data Preparation
}
\description{
Prepares a neighborhood data from a given image.
}
\usage{
dataPrep(image, nb, vars = NULL)
}
\arguments{
  \item{image}{
     the given image in the matrix format.
}
  \item{nb}{
     the size of the neighborhood. It must be either a scalar or a vector of length 3.
}
  \item{vars}{
     names of all the variables that are to be included in the neighborhood data (e.g., vars=c("V2","V3")).
}
}
\value{
A dataframe with column names "V1", "V2", "V3", ...
The first column "V1" contains the response pixel, whereas the other columns contain pixels in the neighborhood (with size \code{nb}) of the response pixel.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\note{
This function uses the left-to-right top-to-bottom raster scan order (see Bui and Apley 2017). Rotating the image by every 90 degrees could be used to quickly change to some other raster scan orders.
}
\seealso{
\code{\link{surfacemodel}}
}
\examples{
## construct the neighborhod data from a simulated image
img <- sarGen(m = 100, n = 100, border = 50)
dat <- dataPrep(img,1)

## to rotate an image by 90 degrees clockwise, use:
img2 <- as.matrix(t(apply(img , 2, rev)))
}
