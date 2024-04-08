npregtol.int <- function (x, y, y.hat, side = 1, alpha = 0.05, P = 0.99,
                           method = c("WILKS", "WALD", "HM"), 
                           upper = NULL, lower = NULL,
                           new = FALSE) 
{
  method <- match.arg(method)
  upper.set <- upper
  lower.set <- lower
  
  xy.data <- cbind(y,x)
  n <- dim(xy.data)[1]
  
  if (length(y.hat) != n) {
    stop(paste("The predictor vector, response vector, and fitted value vector must all be of the same length!", 
               "\n"))
  }
  
  res <- y - y.hat
  tol.temp <- nptol.int(res, side = side, alpha = alpha, P = P, 
                        method = method, upper = NULL, lower = NULL)
  
  tol.upper <- y.hat + as.numeric(tol.temp[4])
  tol.lower <- y.hat + as.numeric(tol.temp[3])
  
  if (!is.null(upper.set)){
    tol.lower[which(tol.lower >= upper.set)] <- upper.set
    tol.upper[which(tol.upper >= upper.set)] <- upper.set
  }
  if (!is.null(lower.set)){
    tol.lower[which(tol.lower <= lower.set)] <- lower.set
    tol.upper[which(tol.upper <= lower.set)] <- lower.set
  }
  
  temp <- as.data.frame(cbind(alpha, P, y, y.hat, 
                              tol.lower, 
                              tol.upper))
  if (side == 2) {
    colnames(temp) <- c("alpha", "P", "y", "y.hat", 
                        "2-sided.lower", "2-sided.upper")
  } else if (side == 1) {
    colnames(temp) <- c("alpha", "P", "y", "y.hat", 
                        "1-sided.lower", "1-sided.upper")
  }  
  
  if (new){
    out.temp <- list()
    out.temp$tol <- temp[,3:6]
    out.temp$alpha.P.side <- c(alpha,P,side)
    out.temp$reg.type  <- "npreg"
    out.temp$method <- method
    if (is.null(lower.set)){lower.set <- NA}
    if (is.null(upper.set)){upper.set <- NA}
    out.temp$lower.upper <- data.frame(Lower = lower.set , Upper = upper.set)
    out.temp
  } else{
    index <- which(names(temp) == "y.hat")
    temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
    temp
  }  
}

