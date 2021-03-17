regtol.int <- function (reg, new.x = NULL, side = 1, alpha = 0.05, P = 0.99) 
{
  if (side != 1 && side != 2) {
    stop(paste("Must specify a one-sided or two-sided procedure!", 
               "\n"))
  }
  if (class(reg) != "lm") {
    stop(paste("Input must be of class 'lm'.", "\n"))
  }
  n <- length(reg$res)
  pars <- length(reg$coef)
  new.length <- 0
  est <- predict(reg, se.fit = TRUE)
  est.1 <- NULL
  if (is.null(new.x) == FALSE) {
    new.length <- nrow(new.x)
    est.1 <- predict(reg, newdata = new.x, se.fit = TRUE)
  }
  y <- c(reg$model[, 1], rep(NA, new.length))
  y.hat <- c(est$fit,est.1$fit)
  se.y <- c(est$se.fit,est.1$se.fit)
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
    temp <- data.frame(cbind(alpha, P, y, y.hat, lower, upper))
    colnames(temp) <- c("alpha", "P", "y", "y.hat", "1-sided.lower", 
                        "1-sided.upper")
  }
  else {
    K <- sqrt(df * qchisq(P, 1, 1/n.star)/qchisq(alpha, df))
    upper <- y.hat + sqrt(MSE) * K
    lower <- y.hat - sqrt(MSE) * K
    temp <- data.frame(cbind(alpha, P, y, y.hat, lower, upper))
    colnames(temp) <- c("alpha", "P", "y", "y.hat", "2-sided.lower", 
                        "2-sided.upper")
  }
  index <- which(names(temp) == "y.hat")
  temp <- data.matrix(temp[order(temp[, index]), ], rownames.force = FALSE)
  temp <- data.frame(temp, check.names = FALSE)
  temp
}

