npmvtol.region <- function(x, alpha = NULL, P = NULL, Beta = NULL, depth.fn, adjust = c("no", "floor", "ceiling"),
                           type = c("central", "semispace"), semi.order = list(lower = NULL, center = NULL, upper = NULL), 
                           L = -Inf, U = Inf, ...){
  if(!((!is.null(alpha)&!is.null(P)&is.null(Beta))|(is.null(alpha)&is.null(P)&!is.null(Beta)))){
    stop(paste("Either alpha and P, or Beta must be specified!", 
               "\n"))
  }
  adjust <- match.arg(adjust)
  type <- match.arg(type)
  n <- nrow(x)
  p <- ncol(x)
  type.vec <- rep(type,p)
  if(type=="semispace"){
    tmp <- unlist(semi.order)
    if(!identical(as.numeric(1:p),sort(as.numeric(tmp)))){
      stop(paste("All indices must be uniquely specified in the list semi.order!","\n"))
    }
    type.vec[semi.order$lower] <- "lower"
    type.vec[semi.order$center] <- "central"
    type.vec[semi.order$upper] <- "upper"
  }
  if(p<2) stop(paste("This procedure only works when x has two or more dimensions!","\n"))
  side <- ifelse(type=="central",2*p,sum(sapply(semi.order,length)*c(1,2,1)))
  if(is.null(Beta)){
    rn <- n*P + qnorm(1-alpha)*sqrt(n*(P*(1-P)))
    rn <- c(floor(rn),ceiling(rn))
    if(adjust=="no"){
      Pb <- c(pbeta(P,rn[1],n+1-rn[1],lower.tail=FALSE),pbeta(P,rn[2],n+1-rn[2],lower.tail=FALSE))
      m <- n - rn[which.min(abs((1-alpha)-Pb))] - side
    } else if(adjust=="floor"){
      m <- n - rn[1] - side
    } else {
      m <- n - rn[2] - side
    }
  } else{
    rn <- (n+1)*Beta
    rn <- c(floor(rn),ceiling(rn))
    if(adjust=="no"){
      m <- n - rn[which.min(abs(Beta-rn/(n+1)))]
    } else if(adjust=="floor"){
      m <- n - rn[1]
    } else{
      m <- n - rn[2]
    }
  } 
  if(m>0){
    d.x <- depth.fn(x,x,...)
    max.depth <- which(d.x==max(d.x))
    cent.x <- apply(matrix(sapply(1:length(max.depth),function(j) x[max.depth[j],]),nrow=length(max.depth)),2,mean)
    euclidean <- sqrt(apply(matrix(scale(x=x,center=cent.x,scale=FALSE),ncol=p)^2,1,sum))
    all.x <- lapply(1:p,function(i) cbind(x,d.x,1:n,euclidean)[order(x[,i],decreasing=(type.vec[i]=="lower")),])
    for(i in 1:m){
      all.x.red <- lapply(1:p, function(j) matrix(all.x[[j]][c(1*(type.vec[j]=="central"),n-(i-1)),],ncol=p+3))
      new.all.x.red <- do.call(rbind,all.x.red)
      tmp.ind <- which(new.all.x.red[,p+1]==min(new.all.x.red[,p+1]))
      if(length(tmp.ind)>1){
        new.all.x.red <- new.all.x.red[tmp.ind,]
        ind.2.remove <- new.all.x.red[as.numeric(which.max(new.all.x.red[,p+3])),p+2]
      } else ind.2.remove <- as.numeric(new.all.x.red[which.min(new.all.x.red[,p+1]),p+2])
      all.x <- lapply(1:p,function(j) all.x[[j]][-which(all.x[[j]][,p+2]==ind.2.remove),] )
    }
    LIMITS <- t(apply(all.x[[1]][,1:p],2,range))
  } else LIMITS <- t(apply(x,2,range))
  colnames(LIMITS) <- c("Lower","Upper")
  if(type=="semispace"){
    LIMITS[semi.order$upper,1] <- L
    LIMITS[semi.order$lower,2] <- U
  }
  if(is.null(colnames(x))) rownames(LIMITS) <- paste("X",1:p,sep="")
  LIMITS
}
