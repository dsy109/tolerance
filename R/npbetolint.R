npbetol.int <- function (x, Beta = 0.95, side = 1, upper = NULL, lower = NULL) 
{
  n <- length(x)
  x <- sort(x)
  ui <- Beta * (n + 1)
  li <- (n - ui)/2
  n.e <- min(ceiling(Beta * (n + 1)), n)
  n.e2 <- max(floor((n - n.e)/2), 1)
  if (is.null(upper))
    upper <- max(x)
  if (is.null(lower))
    lower <- min(x)
  if (side == 1) {
    Upper <- ifelse(ui>n,upper,x[n.e])
    Lower <- ifelse(n-ui+1<1,lower,x[max(n - n.e + 1, 1)])
  }
  else {
    Upper <- ifelse(ui+li>n,upper,x[min(n.e + n.e2, n)])
    Lower <- ifelse(li<1,lower,x[n.e2])
  }
  temp <- data.frame(cbind(Beta, Lower, Upper))
  colnames(temp) <- c("beta", paste(side, "-sided.lower", sep = ""), 
                      paste(side, "-sided.upper", sep = ""))
  temp
}
