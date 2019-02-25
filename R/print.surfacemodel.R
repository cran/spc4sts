print.surfacemodel <- function(x, ...) {
  cat('Supervised learning model was fitted using:', class(x$fit), '\n')

  if (x$standardize)
    cat('Image standardizaion: Yes\n')
  else
    cat('Image standardizaion: No\n')

  cat('Neighborhood size:', x$nb, '\n')

  if (x$trim.vars)
    cat('Variable trimming: Yes\n')
  else
    cat('Variable trimming: No\n')

  cat('Model complexity parameter:', x$complexity, '\n')

  if (is.null(x$R2cv))
    cat('Cross-validation was not used when fitting the model.\n')
  else
    cat('Model cross-validated R-squared:', x$R2cv, '\n')

  cat('Residual MSE:', x$MSE, '\n')
  invisible(x)
}
