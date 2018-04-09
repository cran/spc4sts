showNb <- function(model, what = c("neighborhood", "predictors", "importance"), plot.it = TRUE) {

  if (is.vector(model)) {
    if (length(model) == 1) {
      h <- w1 <- w2 <- model
    } else {
      if (length(model) == 3) {
        h <- model[1]
        w1 <- model[2]
        w2 <- model[3]
      } else stop("Wrong input for the model argument!")
    }
  } else if (class(model) == "surfacemodel") {
    h <- model$nb[1]
    w1 <- model$nb[2]
    w2 <- model$nb[3]
  }

  fullNb <- matrix('', h + 1, w1 + w2 + 1)
  fullNb[1:((h+1)*(w1+1))] <- c(2:((h+1)*(w1+1)),1)
  fullNb[1:h,(w1+2):(w1+w2+1)] <- ((h+1)*(w1+1)+1):(((w1+w2+1)^2+1)/2)

  what <- match.arg(what)

  if (what != "neighborhood") {
    vars <- names(model$fit$variable.importance)
    temp <- unique(na.omit(as.numeric(unlist(strsplit(unlist(vars), "[^0-9]+")))))
    fullNb[which(!fullNb %in% temp)] <- "   ";

    if (what == "predictors")
      fullNb <- apply(fullNb, 1:2, function(x) if (x != "   ") paste("V", x, sep = "") else "   ")

    if (what == "importance") {
      imp <- model$fit$variable.importance/sum(model$fit$variable.importance)*100
      fullNb <- apply(fullNb, 1:2, function(x)
        if (x != "   ") formatC(imp[which(temp == x)], 1 , format="f") else "   ")
      fullNb[(h+1),(w1+1)] <- " y "
    }
  } else {
    fullNb <- apply(fullNb, 1:2, function(x) if (x != "") paste("V", x, sep = "") else "   ")
  }

  if (plot.it) {
    dev.new(width = 6.5, height = 2)
    par(mar = c(.5, 0, .5, 0))
    grid.table(fullNb)
  }

  invisible(fullNb)

}


