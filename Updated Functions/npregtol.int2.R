npregtol.int2 <- function (x, y, y.hat, side = 1, alpha = 0.05, P = 0.99, method = c("WILKS", 
                                                                                     "WALD", "HM"), upper = NULL, lower = NULL,
                           new = TRUE) 
{
  method <- match.arg(method)
  n <- length(x)
  if (length(x) != n | length(y) != n | length(y.hat) != n) {
    stop(paste("The predictor vector, response vector, and fitted value vector must all be of the same length!", 
               "\n"))
  }
  res <- y - y.hat
  tol.temp <- nptol.int(res, side = side, alpha = alpha, P = P, 
                        method = method, upper = upper, lower = lower)
  out.temp <- list()
  for (i in 1:nrow(tol.temp)) {
    upper <- y.hat + tol.temp[i, 4]
    lower <- y.hat + tol.temp[i, 3]
    temp <- data.frame(cbind(alpha, P, x, y, y.hat, lower, 
                             upper))
    if (side == 2) {
      colnames(temp) <- c("alpha", "P", "x", "y", "y.hat", 
                          "2-sided.lower", "2-sided.upper")
    }
    else {
      colnames(temp) <- c("alpha", "P", "x", "y", "y.hat", 
                          "1-sided.lower", "1-sided.upper")
    }
    if (new){
      out.temp[[i]] <- list()
      out.temp[[i]]$fit <- temp[,4:7]
      out.temp[[i]]$alpha.P <- c(alpha,P)
      out.temp[[i]]$reg.type  <- "nonparametric regression"
    } else{
      index <- which(names(temp) == "y.hat")
      temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
      out.temp[[i]] <- temp
    }
  }
  if (length(out.temp) == 1) {
    out.temp <- out.temp[[1]]
  }
  # out.temp <- data.frame(out.temp, check.names = FALSE)
  out.temp
}