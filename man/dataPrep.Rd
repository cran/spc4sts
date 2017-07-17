\name{dataPrep}
\alias{dataPrep}
\title{
Neighborhood Data Preparation
}
\description{
Prepares a neighborhood data from a given image, using the left-to-right top-to-bottom raster scan order (see Bui and Apley 2017).
}
\usage{
dataPrep(img, nb, vars = NULL)
}
\arguments{
  \item{img}{
     the given image in the matrix format.
}
  \item{nb}{
     the size of the neighborhood. It must be a positive vector of length 1 or 3.
}
  \item{vars}{
     names of all the variables that are to be included in the neighborhood data (e.g., vars = c("V2", "V3")).
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
\seealso{
\code{\link{surfacemodel}, \link{localStat}}
}
\examples{
## construct the neighborhod data from a simulated image
img <- sarGen(m = 100, n = 100, border = 50)
dat <- dataPrep(img, 1)
}
