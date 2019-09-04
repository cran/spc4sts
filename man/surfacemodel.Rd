\name{surfacemodel}
\alias{surfacemodel}
\alias{print.surfacemodel}
\alias{plot.surfacemodel}
\title{
Statistical reprentations of stochastic textured surfaces using supervised learning
}
\description{
Provides a statistical represenation for a given stochastic textured surface image via a supervised learning model (a regression tree in this version).
}
\usage{
surfacemodel(img, nb, trim.vars = TRUE, cp = 1e-5,
             xval = 5, standardize = TRUE, subsample = 1,
             verbose = FALSE, keep.residuals = FALSE,
             stationary.test = FALSE, conf.level = .95, nsamples = 20)
}
\arguments{
  \item{img}{
the given stochastic textured surface image in the matrix format.
}
  \item{nb}{
the size of the neighborhood. It must be a 1-length or 3-length vector of positive integer(s). If the former, it is the same with a 3-length vector with the same elements.
}
\item{trim.vars}{
if \code{TRUE}, refit the model using only the variables that were used in the first fit.
}
  \item{cp}{
the complexity parameter for rpart fits (see \code{\link{rpart.control}}).
}
  \item{xval}{
the number of folds in cross-validation (see \code{\link{rpart.control}}). If \code{xval <= 1}, cross-validation will not be used.
}
  \item{standardize}{
if \code{TRUE}, standardize the given image \code{img <- (img - mean(img))/sd(img)}. This reduces the effect of different lighting conditions when images are taken.
}
  \item{subsample}{
the portion of pixels in the given image \code{img} to be used. It takes values in (0,1].  \code{subsample = .5} means that roughly a half number of pixels is used.
}
  \item{verbose}{
if \code{TRUE}, output some model fitting information.
}
\item{keep.residuals}{
if \code{TRUE}, keep residuals of the fitted model in the output.
}
\item{stationary.test}{
if \code{TRUE}, perform a stationary test for the given image \code{img}.
}
\item{conf.level}{
the confidence level of the stationary test. If the \emph{p}-value of the test is not greater than \code{1 - conf.level}, the function will stop and return the \emph{p}-value.
}
\item{nsamples}{the number of bootstrap samples in the stationary test.}
}
\value{
A \code{surfacemodel} object containing the following components:
\item{fit }{the pruned \code{rpart} tree using cross-validation.}
\item{trim.vars}{the \code{trim.vars} argument.}
\item{nb }{the \code{nb} argument.}
\item{Fr }{the empirical cdf with exponential tail approximation of the model residuals.}
\item{MSE }{the mean squared residuals.}
\item{standardize }{the \code{standardize} argument.}
\item{R2cv }{the cross-validated R-squared of \code{fit}.}
\item{complexity }{the complexity value of the returned \code{fit}.}
\item{vars }{the variables used in the formula when fitting the model.}
\item{p.value }{the p-value of the stationary test.}
\item{nsamples }{the number of bootstrap samples in the stationary test.}
\item{residuals }{the residuals of the fitted model.}
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\note{
The best value for the neighborhood size \code{nb} argument can be chosen by comparing the cross-validated R-squared values \code{R2cv} of models built with different values of \code{nb}. Users may use `surfacemodel` with some initial large \code{nb}, and then use the \code{showNb()} function to visualize the importance of the predictors used in the fitted model to have some idea about the range of important predictors to reduce (or increase if necessary) \code{nb}.

After finalizing the choice of \code{nb}, it is better to set \code{trim.vars = TRUE} to further remove some unused variables within that neighborhood.

The raster scan order for constructing the neiborhood data in \code{dataPrep()} is left-to-right then top-to-bottom (see Bui and Apley 208a). Rotating the image by every 90 degrees could be used to quicly change to some other raster scan orders. Again, the cross-validated R-squared \code{R2cv} output can be used to select the best raster scan order. See the below examples.

The stationary test is performed by using the \code{\link{TOS2D}} function in the \code{LS2Wstat} package.

\code{plot.surfacemodel()} is a generic function for \code{surfacemodel()} that produces two plots: a plot of the cross-validation R-squared against the complexity parameter and a histogram of the residuals (along with a normal density curve) of the fitted model.
}
\seealso{
\code{\link{dataPrep}, \link{showNb}, \link{monitoringStat}, \link{rpart}}
}
\examples{
## fit a model to characterize the surface of a simulated image:
img <- sarGen(m = 50, n = 50, border = 50) # training image
model <- surfacemodel(img, nb = 1, keep.residuals = TRUE) # see Note above for how to select nb
model
# plot cross-validation R-squared against complexity parameter and residual histogram
plot(model, type=1:2)

## change the raster scan order from left-to-right then top-to-bottom to
## left-to-right then bottom-to-top, and re-fit the model
## (see the Note section above)
img2  <- as.matrix(t(apply(img , 2, rev)))
model2 <- surfacemodel(img2, nb = 1)
model2$R2cv # cross-validation R-squared
}
