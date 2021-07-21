nlregtol.int2 <- function (formula, xy.data = data.frame(), new.x = NULL, side = 1, 
                           alpha = 0.05, P = 0.99, maxiter = 50, new=TRUE, ...) 
{
  n <- nrow(xy.data)
  form <- as.formula(formula)
  out <- try(suppressWarnings(nls(formula = form, data = xy.data, 
                                  control = list(maxiter = maxiter, warnOnly = TRUE), ...)), 
             silent = TRUE)
  test.sig <- class(try(summary(out)$sigma, silent = TRUE))
  if (test.sig == "try-error") {
    stop(paste("Error in nls routine.  Consider different starting estimates \n\tof the parameters.  Type help(nls) for more options."), 
         call. = FALSE)
  }
  
  sigma <- summary(out)$sigma
  if (sigma == Inf | sigma == (-Inf)){
    warning("Residual sum of square of Nonlinear regression is NOT finite.")
  }
  
  beta.hat <- coef(out)
  beta.names <- names(beta.hat)
  temp <- data.frame(matrix(beta.hat, ncol = length(beta.hat)))
  colnames(temp) <- beta.names
  pars <- length(beta.hat)
  (fx <- deriv(form, beta.names))
  P.mat <- with(temp, attr(eval(fx), "gradient"))
  PTP <- t(P.mat) %*% P.mat
  PTP2 <- try(solve(PTP), silent = TRUE)
  test.PTP <- class(PTP2)[1]
  if (test.PTP == "try-error") {
    PTP0 <- PTP
    while (test.PTP == "try-error") {
      PTP3 <- PTP0 + diag(rep(min(diag(PTP))/1000, length(diag(PTP))))
      PTP.new <- try(solve(PTP3), silent = TRUE)
      test.PTP <- class(PTP.new)[1]
      PTP0 <- PTP3
    }
    PTP <- PTP.new
  } else {PTP <- PTP2}
  
  if (is.null(new.x) == FALSE) {
    new.x <- as.data.frame(new.x)
    if (dim(new.x)[2] != (dim(xy.data)[2]-1)){
      stop("Dimension of new data needs to be the same as the original dataset")
    }
    
    x.temp <- cbind(NA, new.x)
    colnames(x.temp) <- colnames(xy.data)
    xy.data <- rbind(xy.data, x.temp)
    P.mat <- with(temp, attr(eval(fx, xy.data), "gradient"))
  }
  
  y.hat <- predict(out, newdata = xy.data)
  n.star <- rep(NULL, nrow(xy.data))
  for (i in 1:nrow(xy.data)) {
    n.star[i] <- c(as.numeric((t(P.mat[i, ]) %*% PTP %*% 
                                 t(t(P.mat[i, ])))))
  }
  
  n.star <- n.star^(-1)
  df = n - pars
  if (side == 1) {
    z.p <- qnorm(P)
    delta <- sqrt(n.star) * z.p
    t.delta <- suppressWarnings(qt(1 - alpha, df = n - pars, 
                                   ncp = delta))
    t.delta[is.na(t.delta)] <- Inf
    K <- t.delta/sqrt(n.star)
    K[is.na(K)] <- Inf
    upper <- y.hat + sigma * K
    lower <- y.hat - sigma * K
    temp <- as.data.frame(cbind(alpha, P, y.hat, xy.data[, 1], 
                                lower, upper))
    colnames(temp) <- c("alpha", "P", "y.hat", "y", "1-sided.lower", 
                        "1-sided.upper")
  }
  else {
    K <- sqrt(df * qchisq(P, 1, 1/n.star)/qchisq(alpha, df))
    upper <- y.hat + sigma * K
    lower <- y.hat - sigma * K
    temp <- as.data.frame(cbind(alpha, P, y.hat, xy.data[, 1], 
                                lower, upper))
    colnames(temp) <- c("alpha", "P", "y.hat", "y", "2-sided.lower", 
                        "2-sided.upper")
  }
  if (new){
    temp2 <- list()
    temp2$tol <- temp[,c(4,3,5,6)]
    temp2$alpha.P <- c(alpha,P)
    temp2$reg.type <- "nonlinear_regression"
    temp2$model <- formula
    temp2$newdata <- as.data.frame(new.x)
    temp2
  } else {
    index <- which(names(temp) == "y")
    temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
    temp <- data.frame(temp, check.names = FALSE)
    temp
  }
}
