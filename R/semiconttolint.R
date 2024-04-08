semiconttol.int <- function(x, alpha = 0.05, P = 0.99, N = 1000){

  #Vectorized version of gfq_pi
  gfq_pi.n <- function(n, x, N){
    d <- rbinom(n, 1, 0.5)
    if(x == 0){
      (1-d)*rbeta(n, 1, N-1)
    } else if(x < N){
      d*rbeta(n, x, N-x+1) + (1-d)*rbeta(n, x+1, N-x)
    } else{
      d*rbeta(n, N, 1) + (1-d)
    }
  }
  # function of difference between result and theoritical percentile
  diff_perc <- function(alpha, lambda, n, t_stat){
    kappa1 <- log(n) + digamma(alpha) - digamma(n*alpha)
    kappa2 <- trigamma(alpha) / n - trigamma(alpha*n)
    kappa3 <- psigamma(alpha, deriv = 2) / (n^2) - psigamma(alpha*n, deriv = 2)
    kappa4 <- psigamma(alpha, deriv = 3) / (n^3) - psigamma(alpha*n, deriv = 3)
    kappa5 <- psigamma(alpha, deriv = 4) / (n^4) - psigamma(alpha*n, deriv = 4)
    dkappa3 <- kappa3 * (kappa2)^(-3/2)
    dkappa4 <- kappa4 * (kappa2)^(-4/2)
    dkappa5 <- kappa5 * (kappa2)^(-5/2)
    z_lambda <- qnorm(lambda)
    Q <- z_lambda + 1/6*dkappa3*(z_lambda^2 - 1) +
      1/24*dkappa4*(z_lambda^3-3*z_lambda)-
      1/36*(dkappa3)^2*(2*z_lambda^3-5*z_lambda)+
      1/120*(dkappa5)*(z_lambda^4-6*z_lambda^2+3)-
      1/24*(dkappa3)*(dkappa4)*(z_lambda^4-5*z_lambda^2+2)+
      1/324*(dkappa3)^3*(12*z_lambda^4-53*z_lambda^2+17)
    result <- kappa1 + sqrt(kappa2)*Q - t_stat
    result
  }
  # function to find shape parameter based on bisection method
  find_alpha <- function(lambda, n, t_stat){
    lb <- 0
    ub <- 15
    df <- diff_perc(ub, lambda, n, t_stat)
    while(df < 0){
      ub <- ub + 1
      df <- diff_perc(ub, lambda, n, t_stat)
    }
    new_value <- (lb + ub) / 2
    df <- diff_perc(new_value, lambda, n, t_stat)
    while(abs(ub-lb)>1e-6){
      if(df > 0){
        ub <- new_value
      } else{
        lb <- new_value
      }
      new_value <- (ub + lb) / 2
      df <- diff_perc(new_value, lambda, n, t_stat)
    }
    return(new_value)
  }
  
  # function to find shape parameter based on R uniroot
  find_alpha_uniroot <- function(lambda, n, t_stat){
    uniroot(diff_perc, lambda = lambda, n = n, t_stat = t_stat,
            c(1e-06, 10))$root
  }
  
  n <- length(x)
  n0 <- sum(x==0)
  n1 <- n-n0
  x.1 <- x[x>0]
  x.bar <- mean(x.1)
  x.tilde <- mean(log(x.1))
  s.x <- sd(x.1)
  s.x.tilde <- sd(log(x.1))
  T.stat <- x.tilde-log(x.bar)
  pi.star <- gfq_pi.n(N,n0,n)
  U.star <- runif(N)
  alpha.star<- sapply(U.star, find_alpha, n=n1, t_stat=T.stat)
  V.star <- rchisq(N,2*n1*alpha.star)
  beta.star <- V.star/(2*n1*x.bar)
  M.star <- (1-pi.star)*alpha.star/beta.star
  eta.star <- (P-pi.star)/(1-pi.star)
  qp.star <- qgamma(eta.star,shape=alpha.star, rate=beta.star)
  M.na <- sum(is.na(M.star))
  qp.na <- sum(is.na(qp.star))
  ZIG.CI <- quantile(M.star,c(alpha/2,1-alpha/2),na.rm=T)
  ZIG.TI <- quantile(qp.star,1-alpha,na.rm=T)
  
  Qd <- qbeta(U.star*pbeta(P,n0+.5,n1+.5),n0+.5,n1+.5)
  
  U1 <- sqrt(rchisq(N, n1-1)/(n1-1))
  U1.2 <- U1^2
  N.rnorm <- rnorm(N)
  ZILN_qp.star <- qgamma((P-pi.star)/(1-pi.star),shape=alpha.star, rate=beta.star)
  ZILN_qp.na <- sum(is.na(ZILN_qp.star))
  ZILN.CI <- exp(quantile(log(1-pi.star)+x.tilde-(N.rnorm/U1)*(s.x.tilde/sqrt(n1))+(.5*s.x.tilde^2/U1.2), c(alpha/2,1-alpha/2), na.rm=T ))
  ZILN.TI <- exp(x.tilde + s.x.tilde*(quantile((N.rnorm+qnorm(eta.star)*sqrt(n1))/U1, 1-alpha, na.rm=T)/sqrt(n1)))
  
  delta <- rbinom(N,1,pi.star)
  ZIG.PI <- quantile((1-delta)*rgamma(N,shape=alpha.star, rate=beta.star),1-alpha, na.rm=T)
  ZILN.PI <- quantile((1-delta)*exp(rnorm(N,x.tilde,s.x.tilde)),1-alpha, na.rm=T)
  
  eta.5 <- (P-qbeta(.5,n0+.5,n1+.5))/(1-qbeta(.5,n0+.5,n1+.5))
  ncp <- qnorm(eta.5)*sqrt(n1)
  x.3 <- x.1^(1/3)
  ZIG.TI.appx <- suppressWarnings((mean(x.3)+ qt(1-alpha,df=n1-1,ncp=ncp)*sd(x.3)/sqrt(n1))^3)
  ZILN.TI.appx <- suppressWarnings(exp(x.tilde + qt(1-alpha,df=n1-1,ncp=ncp)*s.x.tilde/sqrt(n1)))
  names(ZIG.TI.appx) <- names(ZIG.TI)
  names(ZILN.TI.appx) <- names(ZILN.TI)
  
  out <- list(ZIG.CI=ZIG.CI,ZIG.PI=ZIG.PI,ZIG.TI=ZIG.TI,ZIG.TI.appx=ZIG.TI.appx,ZILN.CI=ZILN.CI,ZILN.PI=ZILN.PI,ZILN.TI=ZILN.TI,ZILN.TI.appx=ZILN.TI.appx,"NA"=c(M.na,qp.na,ZILN_qp.na))
  out
  
}