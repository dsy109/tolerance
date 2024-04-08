nlregtol.int <- function (formula, xy.data = data.frame(), x.new = NULL, side = 1, 
                           alpha = 0.05, P = 0.99, maxiter = 50, new = FALSE, ...) 
{
  xy.data.original <- xy.data
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
  P.mat <- with(temp, attr(eval(fx, xy.data.original), "gradient"))
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
  
  if (is.null(x.new) == FALSE) {
    x.new <- as.data.frame(x.new)
    if (dim(x.new)[2] != (dim(xy.data)[2]-1)){
      stop("Dimension of new data needs to be the same as the original dataset")
    }
    
    x.temp <- cbind(NA, x.new)
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
    temp2$alpha.P.side <- c(alpha,P,side)
    temp2$reg.type <- "nlreg"
    temp2$model <- formula
    temp2$newdata <- as.data.frame(x.new)
    temp2$xy.data.original <- xy.data.original
    temp2
  } else {
    index <- which(names(temp) == "y")
    temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
    temp <- data.frame(temp, check.names = FALSE)
    temp
  }
}


############################
## 95%/95% 2-sided nonlinear regression tolerance bounds
## for a sample of size 50.
#set.seed(100)
#x <- runif(50, 5, 45)
#f1 <- function(x, b1, b2) b1 + (0.49 - b1)*exp(-b2*(x - 8)) +
#  rnorm(50, sd = 0.01)
#y <- f1(x, 0.39, 0.11)
#formula <- as.formula(y ~ b1 + (0.49 - b1)*exp(-b2*(x - 8)))
#out1 <- nlregtol.int2(formula = formula,
#                      xy.data = data.frame(cbind(y, x)),
#                      x.new=c(10,20), side = 2,
#                      alpha = 0.05, P = 0.95 , new = TRUE)
#out1
#########
#set.seed(100)
#x1 <- runif(50, 5, 45)
#x2 <- rnorm(50, 0, 10)
#f1 <- function(x1, x2, b1, b2) {(0.49 - b1)*exp(-b2*(x1 + x2 - 8)) +
#    rnorm(50, sd = 0.01)}
#y <- f1(x1 , x2 , 0.25 , 0.39)
#formula <- as.formula(y ~ (0.49 - b1)*exp(-b2*(x1 + x2 - 8)))
#out2 <- nlregtol.int2(formula = formula,
#                      xy.data = data.frame(cbind(y, x1 , x2)),
#                      x.new=cbind(c(10,20) , c(47 , 53)), side = 2,
#                      alpha = 0.05, P = 0.95 , new = TRUE)
#out2
