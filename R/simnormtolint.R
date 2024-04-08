simnormtol.int <- function(x, alpha = 0.05, P = 0.99, side = 1, 
                           method = c("EXACT", "BONF"), m = 50, log.norm = FALSE) 
{
  if(inherits(x , "matrix") & !inherits(x , "list")){
    stop(paste("The data must be given as a matrix or list.", 
               "\n"))
  }
  if(log.norm){
    if(is.matrix(x)) x <- log(x) else x <- lapply(x,log)
  }
  if(is.matrix(x)){
    x.bar <- apply(x,2,mean)
    s2.bar <- apply(x,2,var)
    n <- nrow(x)
    l <- ncol(x)
    sp <- sqrt(sum((n-1)*s2.bar)/(n*l - l))
  } else{
    if(method=="EXACT"){
      stop(paste("The exact procedure is only available for equal sample sizes.", 
                 "\n"))
    }
    x.bar <- sapply(x,mean)
    s2.bar <- sapply(x,var)
    n <- sapply(x,length)
    l <- length(x)
    sp <- sqrt(sum((n-1)*s2.bar)/(sum(n)-l))
  }
  method <- match.arg(method)
  K <- invisible(K.factor.sim(n = n, l = l, alpha = alpha, P = P, 
                              side = side, method = method, m = m))
  lower <- x.bar - sp * K
  upper <- x.bar + sp * K
  if (log.norm) {
    lower <- exp(lower)
    upper <- exp(upper)
    x.bar <- exp(x.bar)
  }
  if (side == 1) {
    temp <- data.frame(cbind(alpha, P, x.bar, lower, upper))
    colnames(temp) <- c("alpha", "P", "x.bar", "1-sided.lower", 
                        "1-sided.upper")
  }
  else {
    temp <- data.frame(cbind(alpha, P, x.bar, lower, upper))
    colnames(temp) <- c("alpha", "P", "x.bar", "2-sided.lower", 
                        "2-sided.upper")
  }
  rownames(temp) <- paste("Pop.", 1:l,sep="")
  temp
}