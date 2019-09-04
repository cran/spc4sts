print.surfacemodel <- function(x, ...) {

  if (!is.null(x$p.value)) {
    cat('Stationary test was performed by TOS2D\n')
    cat('  H0: the image is stationary\n')
    cat('  Number of bootstrap samples:', x$nsamples,'\n')
    cat('  p-value:', x$p.value,'\n')
  }

  cat('Supervised learning model was fitted using', class(x$fit), '\n')

  if (x$standardize)
    cat('  Image standardization: Yes\n')
  else
    cat('  Image standardization: No\n')

  cat('  Neighborhood size:', x$nb, '\n')

  if (x$trim.vars)
    cat('  Variable trimming: Yes\n')
  else
    cat('  Variable trimming: No\n')

  cat('  Model complexity parameter:', x$complexity, '\n')

  if (is.null(x$R2cv))
    cat('Cross-validation was not used when fitting the model.\n')
  else
    cat('  Model cross-validation R-squared:', x$R2cv, '\n')

  cat('  Residual MSE:', x$MSE, '\n')

  invisible(x)
}
