ddiffprop.alt <- function (x, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5, log = FALSE, 
          ...) 
{
  if ((a1 < 0 | a1 > 1) | (a2 < 0 | a2 > 1)) {
    warning("a1 and a2 should both be between 0 and 1 for this fiducial approach!", 
            call. = TRUE)
  }
  d <- x
  ddiffprop2 <- function(d, k1, k2, n1, n2, a1, a2, ...) {
    c1 <- k1 + a1
    c2 <- k2 + a2
    b1 <- n1 - k1 + a1
    b2 <- n2 - k2 + a2
    K <- beta(c1, b1) * beta(c2, b2)
    if (d >= (-1) & d <= 0) {
      out <- try(beta(c1, b2) * F1(b2, c1 + c2 + b1 + b2 - 
                                     2, 1 - c2, c1 + b2, 1 + d, 1 - d^2, ...) * ((-d)^(b1 + 
                                                                                         b2 - 1) * (1 + d)^(c1 + b2 - 1))/K, silent = TRUE)
    }
    else if (d > 0 & d <= 1) {
      out <- try(beta(c2, b1) * F1(b1, c1 + c2 + b1 + b2 - 
                                     2, 1 - c1, c2 + b1, 1 - d, 1 - d^2, ...) * (d^(b1 + 
                                                                                      b2 - 1) * (1 - d)^(c2 + b1 - 1))/K, silent = TRUE)
    }
    else out <- 0
    if (class(out) == "try-error") 
      out <- -1
    out
  }
  TEMP <- Vectorize(ddiffprop2)
  temp <- TEMP(d = d, k1 = k1, k2 = k2, n1 = n1, n2 = n2, a1 = a1, 
               a2 = a2, ...)
  ind <- which(temp==-1)
#print(min(d[ind]))
#print(TEMP(d = min(d[ind]), k1 = k1, k2 = k2, n1 = n1, n2 = n2, a1 = a1,            a2 = a2))
  if(length(ind)>0){
    dmin <- pmax(min(d[ind])-seq(0.1,0,length=1000),-1)
    dmax <- pmin(max(d[ind])+seq(0,0.1,length=1000),1)
#    print(dmin)
#    print(dmax)
    temp.lower <- TEMP(d = dmin, k1 = k1, k2 = k2, n1 = n1, n2 = n2, a1 = a1, 
                       a2 = a2, ...)
    temp.upper <- TEMP(d = dmax, k1 = k1, k2 = k2, n1 = n1, n2 = n2, a1 = a1, 
                       a2 = a2, ...)
#    print(temp.lower)
#    print(temp.upper)
    ind.lower <- min(which(temp.lower==-1))
    ind.lower <- ind.lower-6:1
    ind.upper <- max(which(temp.upper==-1))
    ind.upper <- ind.upper+1:6
#    print(ind.lower);print(ind.upper)
    TTT <- data.frame(x=c(dmin[ind.lower],dmax[ind.upper]),d.x=c(temp.lower[ind.lower],temp.upper[ind.upper]))
#    print(TTT)
    tmp.out <- lm(I(log(d.x))~x,data=TTT)
    temp[ind] <- as.numeric(exp(predict(tmp.out,newdata=data.frame(x=d[ind]))))
  }
  if (log) 
    temp <- log(temp)
  temp
}

ddiffprop.alt(sort(X), k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0)

pdiffprop.alt <- function (q, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5, lower.tail = TRUE, 
          log.p = FALSE, ...) 
{
  if ((a1 < 0 | a1 > 1) | (a2 < 0 | a2 > 1)) {
    warning("a1 and a2 should both be between 0 and 1 for this fiducial approach!", 
            call. = TRUE)
  }
  x <- q
  pdiffprop2 <- function(x, k1, k2, n1, n2, a1, a2, ...) {
    if (x <= (-1)) {
      out <- 0
    }
    else if (x >= 1) {
      out <- 1
    }
    else {
      out <- integrate(Vectorize(ddiffprop.alt), k1 = k1, k2 = k2, 
                       n1 = n1, n2 = n2, a1, a2, lower = -1, upper = x, 
                       ...)$value
    }
    out
  }
  TEMP <- Vectorize(pdiffprop2)
  temp <- TEMP(x = x, k1 = k1, k2 = k2, n1 = n1, n2 = n2, a1, 
               a2, ...)
  if (lower.tail == FALSE) 
    temp <- 1 - temp
  if (log.p) 
    temp <- log(temp)
  temp
}



set.seed(1)

tolerance::pdiffprop(0, k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0,lower.tail = FALSE)
#[1] 0.9902356

mean(tolerance::rdiffprop(10000, k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0)>0)
#[1] 0.9737

mean((rbeta(10000, shape1 = 5+1, shape2 =14-5+1)-rbeta(10000, shape1 = 3+26,shape2=7-3+155))>0)
#[1] 0.9752

#Note that this is the proportion; the quantity in your original email was 100 times this, or rather a percentage
tolerance::pdiffprop(0, k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0,lower.tail = TRUE)
#[1] 0.009764406


#Matches what you simulated.
pdiffprop.alt(0, k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0,lower.tail = FALSE)
#[1] 0.9772136

pdiffprop.alt(0, k1 = 5+1, k2 = 3+26, n1 =14+2, n2 = 7+155+26,a1=0,a2=0,lower.tail = TRUE)
#[1] 0.02278636




