\name{disMat}
\alias{disMat}
\title{
Pairwise Dissimilarity Matrix of Stochastic Textured Surfaces
}
\description{
Compute KL and ALK dissimiarlity matrices for the given stochastic textured surface images.
}
\usage{
disMat(imgs, nb, cp=1e-3, subsample = c(1, .5),
                   standardize = TRUE, keep.fits = FALSE, verbose=FALSE)
}
\arguments{
  \item{imgs}{
a 3-dimensional array containing all images.
}
  \item{nb}{
the size of the neighborhood. It must be a 1-length or 3-length vector of positive integer(s). If the former, it is the same with a 3-length vector with the same elements.
}
  \item{cp}{
the minimal value for the \code{rpart} complexity models. The smaller \code{cp} is, the more complex the \code{rpart} models are fit.
}
  \item{subsample}{
the portion of pixels in the given image \code{img} to be used when fitting models (the first component) and computing dissimilarities (the second component). It takes values in (0, 1] (e.g., \code{subsample = c(1, .5)} means that the whole image is used when fitting models, and roughly a half of that is used when compute dissimilarities).
}
  \item{standardize}{
if \code{TRUE}, standardize the given image \code{img <- (img - mean(img))/sd(img)}. This reduces the effect of different lighting conditions when images are taken.
}
  \item{keep.fits}{
if \code{TRUE}, save all the fitted models in the "fits.Rdata" under the wokring directory.
}
  \item{verbose}{
if set to \code{TRUE}, output some computational time information.
}
}
\value{
the KL and AKL dissimilarity matrices.
}
\references{
Bui, A.T. and Apley, D.W. (2019b) "An exploratory analysis approach for understanding variation in stochastic textured surfaces", Computational Statistics & Data Analysis, in press. doi:10.1016/j.csda.2019.01.019.
}
\author{
Anh Bui
}
\examples{
## generate images: the first two are similar, the third is different with the other two
phi1 <- c(.6, .6, .5)
phi2 <- c(.35, .35, .3)
imgs <- array(0, c(100,100,3))
for (j in 1:dim(imgs)[3])
  imgs[,,j] <- sarGen(phi1 = phi1[j], phi2 = phi2[j], m = 100, n = 100, border = 50)
## compute KL and AKL dissimilarity matrices
disMat(imgs = imgs, nb = 1)
}
