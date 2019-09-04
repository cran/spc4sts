plot.surfacemodel <- function(x, type=1:2, ...) {

  # plot CV R^2 against cp
  if (1 %in% type) {
    if (!is.null(x$R2cv)) {
      p.rpart <- x$fit$cptable
      #xstd <- p.rpart[, 5L]
      xerror <- p.rpart[, 4L]
      nsplit <- p.rpart[, 2L]
      ns <- seq_along(nsplit)
      cp0 <- p.rpart[, 1L]
      cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
      #ylim <- c(min(xerror - xstd) - 0.1, max(xerror + xstd) + 0.1)
      do.call(plot, c(list(ns, 1-xerror, axes = FALSE, type = "l",
                           xlab = "cp", ylab = "CV R-squared",...)))
      axis(2, cex.axis=1)
      axis(1L, at = ns, labels = as.character(signif(cp, 2L)), cex.axis=1)
    }
    else
      cat('Cannot plot cross-validation R-squared against complexity parameter
          because cross-validation was not used when fitting the given model.
          Set xval>1 when using surfacemodel() to use this feature.')
  }


  # plot residual histogram
  if (2 %in% type) {
    if (!is.null(x$r)) {
      Residuals <- x$r
      hist(Residuals, prob=TRUE, main="", ...)
      curve(dnorm(x, mean=mean(Residuals), sd=sd(Residuals)),
            add=TRUE, col='red', lty=2)
    }
    else
      cat('Residual histogram cannot be plotted because
          residuals are not kept in the given model.
          Set keep.residuals=TRUE when using surfacemodel()
          to keep the residuals.')
  }

}
