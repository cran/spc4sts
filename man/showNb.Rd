\name{showNb}
\alias{showNb}
\title{
Show Neighborhood
}
\description{
Shows the neighborhood corresponding to the left-to-right then top-to-bottom raster scan order with additional information: variable names of the data frame returned by \code{dataPrep}, predictors used in the model returned by \code{surfacemodel}, or their percentage importance in the model (currently extracted from the \code{rpart} object). This function is useful for choosing a good neighborhood size and understanding relationship between pixels (e.g., periodicity).
}
\usage{
showNb(model, what = c("neighborhood", "predictors", "importance"), plot.it = TRUE)
}
\arguments{
  \item{model}{
either the object returned by \code{surfacemodel} or a positive vector of length 1 or 3 specifying the neighboorhood. If it is a vector, \code{what <- "neighborhood"}.
}
  \item{what}{
  what to show in the neighorhood. \code{"neighborhood"} shows variable names of the data frame returned by \code{dataPrep}, \code{"predictors"} shows predictors used in the model returned by \code{surfacemodel}, and \code{"importance"} shows their percentage importance in the model.
  }
  \item{plot.it}{
  if \code{TRUE}, plot the neighborhood.
  }
}
\value{
A matrix that contains the information for the plot (using the \code{grid.table} function).
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, to appear.
}
\author{
Anh Bui
}
\seealso{
\code{\link{dataPrep}, \link{surfacemodel}}
}
\examples{
## show the neighorhood with variables names of the data frame constructed by dataPrep()
img <- matrix(1:25, 5, 5) # an image of size 5x5 pixels
img
dataPrep(img, 2)
showNb(c(2, 2, 2)) # showNb(2) has the same effect

## show the neighorhood with predictors and their importance used in the model returned
## by surfacemodel()
img <- sarGen(m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img, nb = 3)
showNb(model, "predictors") # show predictors
showNb(model, "importance") # show predictor percentage importance
}
