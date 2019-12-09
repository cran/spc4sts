\name{dataPrep}
\alias{dataPrep}
\title{
Neighborhood Data Preparation
}
\description{
Prepares a neighborhood data from a given image, using the left-to-right then top-to-bottom raster scan order (see Bui and Apley 2018a).
}
\usage{
dataPrep(img, nb, vars = NULL, subsample = 1)
}
\arguments{
  \item{img}{
     the given image in the matrix format.
}
  \item{nb}{
     the size of the neighborhood. It must be a 1-length or 3-length vector of positive integer(s). If the former, it is the same with a 3-length vector with the same elements.
}
  \item{vars}{
     names of variables to be selected in the neighborhood data.
}
  \item{subsample}{
the portion of data rows be returned. It takes values in (0, 1]. If \code{subsample = 1}, all data rows will be returned, and if \code{subsample = .5}, only roughly a half will be returned.
}
}
\value{
A dataframe with column names "V1", "V2", "V3", ...
The first column "V1" contains the response pixel, whereas the other columns contain pixels in the neighborhood (with size \code{nb}) of the response pixel.
}
\note{
Only rows without missing values (corresponding to pixels with full neighborhood) are returned.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{surfacemodel}, \link{monitoringStat}}
}
\examples{
## construct a neighborhood data for an unrealistically small mock image (7x9 pixels).
mock.img <- matrix(sample(0:255, 63, replace = TRUE), 7, 9)
mock.img
dataPrep(img = mock.img, nb = 2) # the same with nb = c(2, 2, 2)

## select only columns "V2", "V5", and "V13" in the output
dataPrep(img = mock.img, nb = 2, vars = c("V2", "V5", "V13"))

## return only a half number of rows
dataPrep(img = mock.img, nb = 2, subsample = .5)
}
