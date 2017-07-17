\name{surfacemodel}
\alias{surfacemodel}
\title{
Statistical reprentations of stochastic textured surfaces using supervised learning.
}
\description{
Provides a statistical represenation for a given stochastic textured surface image via a supervised learning model (a regression tree in this version).
}
\usage{
surfacemodel(img, nb, trim.vars = TRUE, control, y = FALSE)
}
\arguments{
  \item{img}{
the given stochastic textured surface image
}
  \item{nb}{
     the size of the neighborhood. It must be a positive vector of length 1 or 3.
}
\item{trim.vars}{
if \code{TRUE}, refit the model using only the variables that were used in the first fit.
}
  \item{control}{
control for rpart fits (see \code{\link{rpart.control}}). The default is \code{rpart.control(minsplit=10, cp=0.00001, xval=10)}.
}
  \item{y}{
if set to \code{TRUE}, keeps a copy of the response in the output.
}
}
\value{
A list of the following:
\item{fit }{the pruned \code{rpart} tree using cross-validation}
\item{trim.vars}{the \code{trim.vars} argument}
\item{R2cv }{the cross-validated R-squared of \code{fit}}
\item{nb }{the argument \code{nb}}
\item{Fr }{the empirical cdf with exponential tail approximation of the model residuals}
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\note{
The best value for the neighborhood size \code{nb} argument can be chosen by comparing the cross-validated R-squared \code{R2cv} output. Users may use `surfacemodel` with some initial large `nb`, and then use the \code{showNb()} function to visualize the importance of the predictors used in the fitted model to have some idea about the range of important predictors to reduce (or increase if necessary) \code{nb}.

After finalizing the choice of \code{nb}, it is better to set \code{trim.vars = TRUE} to further remove some unused variables within that neighborhood.

The raster scan order for constructing the neiborhood data in \code{dataPrep()} is left-to-right top-to-bottom (see Bui and Apley 2017). Rotating the image by every 90 degrees could be used to quicly change to some other raster scan orders. Again, the cross-validated R-squared \code{R2cv} output can be used to select the best raster scan order. See the below examples.

}
\seealso{
\code{\link{dataPrep}, \link{showNb}, \link{localStat}, \link{rpart}}
}
\examples{
## fit a model to characterize the surface of a simulated image:
img <- sarGen(m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img, nb = 1) # see Note above for how to select nb
model$R2cv

## change the raster scan order from left-to-right top-to-bottom to
## left-to-right bottom-to-rop, and re-fit the model (see the Note section above)
img2  <- as.matrix(t(apply(img , 2, rev)))
model2 <- surfacemodel(img2, nb = 1)
model2$R2cv # expected to be smaller than model$R2cv
}
