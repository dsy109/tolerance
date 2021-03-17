mvregtol.region <- function (mvreg, new.x = NULL, alpha = 0.05, 
                             P = 0.99, B = 1000) 
{
  if(class(mvreg)[1]!="mlm"){
    stop(paste("'mvreg' must be of class 'mlm'!", "\n"))
  }
  X <- model.matrix(mvreg)
  n <- nrow(X)
  q <- ncol(mvreg$coefficients)
  y.vars <- colnames(mvreg$coefficients)
  if(all(X[,1]==1)){
    m <- ncol(X)-1
    x <- as.matrix(X[,-1])
    x.vars <- colnames(X)[-1]
  } else{
    m <- ncol(X)
    x <- as.matrix(X)
    x.vars <- colnames(X)
  }
  x.bar <- apply(x, 2, mean)
  
  f.m <- n - m - 1
  P.n <- matrix(1, n, n)
  P.n <- diag(1, n) - (P.n/n)
  x1 <- data.frame(x)
  colnames(x1) <- x.vars
  y.hat <- predict(mvreg)
  if(!is.null(new.x)){
    nn <- nrow(new.x)
    new.xy <- data.frame(matrix(1:(q*nn),ncol=q),new.x)
    colnames(new.xy)[1:q] <- y.vars
    y.hat <- rbind(y.hat,predict(mvreg,newdata=new.xy))    
    x.terms <- as.formula(paste("~",paste(attr(mvreg$terms,"term.labels"),collapse="+"),"-1"))
    new.x <- model.matrix(x.terms,data=new.xy)
  }
  x.all <- as.matrix(rbind(x1, new.x))
  N <- nrow(x.all)
  A <- ginv(t(x) %*% P.n %*% x)

  d.2 <- (1/n) + sapply(1:N, function(i) rbind(x.all[i, ] - 
                                                 x.bar) %*% A %*% cbind(x.all[i, ] - x.bar))
  H.2 <- lapply(1:B, function(i) matrix(rchisq(N * q, df = 1), 
                                        ncol = q) * d.2)
  L <- t(sapply(1:B, function(i) eigen(rwishart(f.m, q))$values))
  c1 <- sapply(1:B, function(i) apply((1 + H.2[[i]]^2)/L[[i]], 
                                      1, sum))
  c2 <- sapply(1:B, function(i) apply((1 + 2 * (H.2[[i]])^2)/(L[[i]])^2, 
                                      1, sum))
  c3 <- sapply(1:B, function(i) apply((1 + 3 * (H.2[[i]])^2)/(L[[i]])^3, 
                                      1, sum))
  a <- (c2^3)/(c3^2)
  T.all <- lapply(1:N, function(i) f.m * ((sqrt(c2[i, ])/a[i, 
                                                           ]) * (qchisq(P, a[i, ]) - a[i, ]) + c1[i, ]))
  k <- sapply(T.all, quantile, 1 - alpha)
  cat("These are ", (1 - alpha) * 100, "%/", P * 100, "% tolerance factors.", 
      sep = "", fill = TRUE)
  tol <- cbind(k, data.frame(y.hat), data.frame(x.all))
  y.vars <- paste(y.vars, ".hat", sep = "")
  tol.col <- c("k.factor", y.vars, x.vars)
  colnames(tol) <- tol.col
  tol
}
