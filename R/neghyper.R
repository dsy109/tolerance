dnhyper <- function(x, m, n, k, log = FALSE){
    if (k > m) {
        stop(paste("k cannot be larger than m!",
            "\n"))
    }
    p <- sapply(1:length(x), function(i) suppressWarnings(dhyper(x=k-1,m=m,n=n-m,k=x[i]-1)*(m-(k-1))/(n-(x[i]-1))))
    p[is.nan(p)] <- 0
    p <- pmin(pmax(p, 0),1)
    if (log)
        p <- log(p)
    p
}

pnhyper <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
{
  if (k > m) {
    stop(paste("k cannot be larger than m!", "\n"))
  }
  p <- phyper(q = k-1, m = m, n = n-m, k = q, lower.tail = !lower.tail, log.p = log.p)
  return(p)
}

qnhyper <- function(p, m, n, k, lower.tail = TRUE, log.p = FALSE){
    if (k > m) {
        stop(paste("k cannot be larger than m!",
            "\n"))
    }
    if (log.p) p <- exp(p)
    all.p <- NULL
    temp <- k:(n - m + k)
    if(lower.tail){
        temp.out <- rbind(c(k,-Inf), cbind(temp, pnhyper(temp, m, n, k)), c(n-m+k,Inf))
    } else temp.out <- rbind(c(k,Inf),cbind(temp, pnhyper(temp, m, n, k, lower.tail = FALSE)),c(n-m+k,-Inf))
    for (i in 1:length(p)) {
    if(lower.tail){
        all.p <- c(all.p, min(temp.out[which(temp.out[,2]>=p[i]),1]))
    } else all.p <- c(all.p, min(temp.out[which(temp.out[,2]<p[i]),1]))
       }
    all.p
}

rnhyper <- function(nn, m, n, k){
    if (k > m) {
        stop(paste("k cannot be larger than m!",
            "\n"))
    }
    qnhyper(runif(nn), m, n, k)
}