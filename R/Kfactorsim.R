K.factor.sim <- function(n, l = NULL, alpha = 0.05, P = 0.99, 
                                  side = 1, method = c("EXACT","BONF"), m = 50) {
  if (side != 1 && side != 2) {
    stop(paste("Must specify a one-sided or two-sided procedure!", 
               "\n"))
  }
  method <- match.arg(method)
  if(method=="EXACT"){
    if (is.null(l)) l <- 1
    if (length(n) != 1) {
      stop(paste("The length of n must be 1.", 
                 "\n"))
    }    
    df = n * l - l
    chi.a <- qchisq(alpha, df)
    k2 <- sqrt(df * qchisq(P, 1, 1/n)/chi.a)
    if(side==1){
      K_one_side_simul_fun <- function(K, n, P, alpha, l, m) {
        fun_temp <- function(z, K, n, P, l) {
          df = n * l - l
          zp = qnorm(P)
          inside = sqrt(n) * (K * sqrt(z) / sqrt(df) - zp) 
          dchisq(z, df) * pnorm(inside)^l
        }
        suppressWarnings(integrate(fun_temp, lower = 0, upper = n * l * 10, #qchisq(.999, n * l - l), 
                                   K = K, n = n, P = P, l = l, 
                                   subdivisions = m)$value - (1 - alpha))
      }
      
      K <- uniroot(f = K_one_side_simul_fun, interval = c(0, k2 + 100), 
                   n = n, P = P, alpha = alpha, l = l,  m = m,
                   tol = .Machine$double.eps^0.5)$root
    } else{
      K_two_side_simul_fun <- function(K, n, P, alpha, l, m) {
        fun_temp <- function(z, K, n, P, l) {
          df = n * l - l
          P1 <- pchisq(df * qchisq(P, df = 1, ncp = z^2/n)/(K^2), df=df, lower.tail=FALSE)
          P2 <- (2*pnorm(z)-1)^(l-1)
          dnorm(z) * P1 * P2
        }
        suppressWarnings(2*l*integrate(fun_temp, lower = 0, upper = n * l * 10, #qchisq(.999, n * l - l), 
                                            K = K, n = n, P = P, l = l, 
                                            subdivisions = m)$value - (1 - alpha))
      }
      
      K <- uniroot(f = K_two_side_simul_fun, interval = c(0, k2 + 100), 
                   n = n, P = P, alpha = alpha, l = l,  m = m,
                   tol = .Machine$double.eps^0.5)$root    
      
    }
  } else{
    if(length(n) > 1 | is.null(l)) l <- length(n)
    if(side==1){
      K <- K.factor(n,f=sum(n)-l,alpha=alpha/l,P=P,side=1)
    } else{
      K <- K.factor(n,f=sum(n)-l,alpha=alpha/l,P=P,side=2,method="EXACT",m=m)
    }
  }
  K
}


