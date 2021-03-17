gamtol.int <- function (x, alpha = 0.05, P = 0.99, side = 1, method = c("HE", "HE2",
                                                                        "WBE", "ELL", "KM", "EXACT", "OCT"), m = 50, log.gamma = FALSE) 
{
  if (side != 1 && side != 2) {
    stop(paste("Must specify a one-sided or two-sided procedure!", 
               "\n"))
  }
  method <- match.arg(method)
  if (method == "ELL") {
    stop(paste("Must specify either the HE, WBE, or EXACT method!", 
               "\n"))
  }
  x <- x
  if (log.gamma) 
    x <- log(x)
  n <- length(x)
  x.bar <- mean(x)
  x2.bar <- mean(x^2)
  inits <- c(x.bar^2/(x2.bar - x.bar^2), (x2.bar - x.bar^2)/x.bar)
  gamma.ll <- function(x, pars) sum(-dgamma(x, shape = pars[1], 
                                            scale = pars[2], log = TRUE))
  out <- try(suppressWarnings(nlm(gamma.ll, p = inits, x = x)$estimate),silent=TRUE)
  if(class(out)=="try-error"){
    out=inits
    message("Note: Difficulties with obtaining maximum likelihood estimates through the nlm function.  Method of moments estimates are used for calculating the tolerance limits.") 
  }
  a.hat <- out[1]
  b.hat <- out[2]
  x <- x^(1/3)
  x.bar <- b.hat^(1/3) * (exp(lgamma(a.hat + (1/3))-lgamma(a.hat)))
  s <- sqrt(b.hat^(2/3) * (exp(lgamma(a.hat + (2/3))-lgamma(a.hat))) - 
              (x.bar)^2)
  if(x.bar==Inf|s==Inf)     stop(paste("Difficulties with estimation. Try rescaling the data.", 
                                       "\n"))
  K <- invisible(K.factor(n = n, alpha = alpha, P = P, side = side, 
                          method = method, m = m))
  lower <- max(0, (x.bar - s * K)^3)
  upper <- (x.bar + s * K)^3
  if (log.gamma) {
    lower <- exp(lower)
    upper <- exp(upper)
    x.bar <- exp(x.bar)
  }
  if (side == 1) {
    temp <- data.frame(cbind(alpha, P, lower, upper))
    colnames(temp) <- c("alpha", "P", "1-sided.lower", "1-sided.upper")
  }
  else {
    temp <- data.frame(cbind(alpha, P, lower, upper))
    colnames(temp) <- c("alpha", "P", "2-sided.lower", "2-sided.upper")
  }
  temp
}
