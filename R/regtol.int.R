regtol.int <- function (reg, new.x = NULL, side = 1, alpha = 0.05, P = 0.99, new = FALSE) 
{
  if (side != 1 && side != 2) {
    stop(paste("Must specify a one-sided or two-sided procedure!", 
               "\n"))
  }
  if (!inherits(reg, "lm")) {
    stop(paste("Input must be of class 'lm'.", "\n"))
  }
  
  n <- length(reg$residuals)
  pars <- length(reg$coefficients)
  new.length <- 0
  est <- predict(reg, se.fit = TRUE)
  est.new <- NULL
  
  if (is.null(new.x) == FALSE) {
    new.x <- as.data.frame(new.x)
    pred.names <- all.vars(formula(reg))[-1]
    if(!all(pred.names%in%colnames(new.x))){
      colnames(new.x)[1:length(pred.names)] <- pred.names
    }
    new.length <- dim(new.x)[1]
    est.new <- predict(reg , newdata = new.x , se.fit = TRUE)
  }
  
  y <- c(reg$model[, 1], rep(NA, new.length))
  y.hat <- as.vector(c(est$fit, est.new$fit))
  se.y <- as.vector(c(est$se.fit, est.new$se.fit))
  a.out <- anova(reg)
  MSE <- a.out$"Mean Sq"[length(a.out$"Mean Sq")]
  df <- a.out$Df[length(a.out$Df)]
  n.star <- MSE/se.y^2
  
  if (side == 1) {
    z.p <- qnorm(P)
    delta <- sqrt(n.star) * z.p
    t.delta <- suppressWarnings(qt(1 - alpha, df = n - pars, 
                                   ncp = delta))
    K <- t.delta/sqrt(n.star)
    upper <- y.hat + sqrt(MSE) * K
    lower <- y.hat - sqrt(MSE) * K
    temp <- as.data.frame(cbind(alpha , P , y, y.hat, lower, upper))
    colnames(temp) <- c("alpha","P","y", "y.hat", "1-sided.lower", 
                        "1-sided.upper")
  }
  else {
    K <- sqrt(df * qchisq(P, 1, 1/n.star)/qchisq(alpha, df))
    upper <- y.hat + sqrt(MSE) * K
    lower <- y.hat - sqrt(MSE) * K
    temp <- as.data.frame(cbind(alpha , P , y, y.hat, lower, upper))
    colnames(temp) <- c("alpha","P","y", "y.hat", "2-sided.lower", 
                        "2-sided.upper")
  }
  
  if(new){
    temp2 <- list()
    temp2$tol <- temp[,3:6]
    temp2$alpha.P.side <- c(alpha,P,side)
    temp2$reg.type  <- "linreg"
    temp2$model <- reg
    temp2$newdata <- as.data.frame(new.x)
    temp2
  } else{
    index <- which(names(temp) == "y.hat")
    temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
    temp <- data.frame(temp, check.names = FALSE)
    temp
  }
}
