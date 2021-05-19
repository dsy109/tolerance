library(ggplot2)
library(scales)
library(tolerance)

ggnorm.OC <- function(k = NULL, alpha = NULL, P = NULL, n, side = 1, 
                      method = c("HE", "HE2", "WBE", "ELL", "KM", "EXACT", "OCT"), 
                      m = 50) {
  if(side != 1 && side != 2){
    stop(paste("Must specify a one-sided or two-sided procedure!", 
               "\n"))
  }  
  if(length(n)<2){
    stop(paste("'n' needs to be a vector of at least length 2 to produce an OC curve.", 
               "\n"))
  }
  n <- sort(n)
  method <- match.arg(method) 
  ### If P is NULL ###
  if(is.null(P)){
    if(length(k)!=1|length(alpha)<1){
      stop(paste("Check values specified for k, n, and alpha!", 
                 "\n"))
    }
    if(length(alpha)>10){
      warning("Too many values of alpha specified!  Using only the first 10 values.",call.=FALSE)
    }
    
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    alpha <- sort(alpha)[1:min(length(alpha))]
    tmp.obj = paste("(k=",k,")",sep="")
    all.P <- cbind(sapply(1:length(alpha), function(i) sapply(1:length(n), function(j) uniroot(function(P) k-K.factor(n=n[j],alpha=alpha[i],P=P,method=method,m=m,side=side),lower=1e-10,upper=1-1e-10)$root)))
    ##########
    data.P <- as.data.frame(cbind(rep(n , dim(all.P)[2]),
                                  as.vector(all.P),
                                  as.vector(matrix(rep(alpha,length(n)) , ncol=length(alpha) , nrow=length(n) , byrow=TRUE))
    ))
    colnames(data.P) <- c("n","P","alpha")
    tmp.obj <- paste("(k=",k,")",sep="")
    ggplot(data = data.P , aes(x=n , y=P , group = 1-alpha)) +
      scale_x_continuous(limits = c(min(n) , max(n)) , breaks = pretty_breaks()) +
      scale_y_continuous(limits = c(min(all.P) , 1) , breaks = pretty_breaks()) +
      xlab("n") + ylab("P") +
      geom_line(aes(colour = factor(1-alpha))) +
      geom_point(aes(colour = factor(1-alpha))) +
      ggtitle(paste("Normal Tolerance Interval OC Curve for P (k=",k,")")) +
      labs(col=(expression(paste("(1-",alpha,")"))))
  } 
  ### If alpha is NULL ###
  else if(is.null(alpha)){
    if(length(k)!=1|length(P)<1){
      stop(paste("Check values specified for k, n, and P!", 
                 "\n"))
    }
    if(length(P)>10){
      warning("Too many values of P specified!  Using only the first 10 values.",call.=FALSE)
    }    
    #dev.new(width=11,height=5)
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    P <- sort(P)[1:min(length(P))]
    all.alpha <- 1-sapply(1:length(P), function(i) sapply(1:length(n), function(j) uniroot(function(alpha) k-K.factor(n=n[j],alpha=alpha,P=P[i],method=method,m=m,side=side),lower=1e-10,upper=1-1e-10)$root))
    ##########
    data.alpha <- as.data.frame(cbind(rep(n , dim(all.alpha)[2]),
                                      as.vector(all.alpha),
                                      as.vector(matrix(rep(P,length(n)) , ncol=length(P) , nrow=length(n) , byrow=TRUE))
    ))
    colnames(data.alpha) <- c("n","alpha","P")
    tmp.obj <- paste("(k=",k,")",sep="")
    ggplot(data = data.alpha , aes(x=n , y=alpha , group = P)) +
      scale_x_continuous(limits = c(min(n) , max(n)) , breaks = pretty_breaks()) +
      scale_y_continuous(limits = c(min(all.alpha) , 1) , breaks = pretty_breaks()) +
      xlab("n") + ylab(expression(paste("(1-",alpha,")"))) +
      geom_line(aes(colour = factor(P))) +
      geom_point(aes(colour = factor(P))) +
      ggtitle(bquote("Normal Tolerance Interval OC Curve for 1-" ~ alpha ~ .(tmp.obj))) +
      labs(col=(expression(paste(P))))
  }
  ### If k is NULL ###
  else if(is.null(k)){
    if((length(P)*length(alpha))>10){
      warning("Too many combinations of alpha and P specified!  Using only the first 10 such combinations.",call.=FALSE)
    }    
    #dev.new(width=11,height=5)
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    alpha <- sort(alpha)
    P <- sort(P)
    tmp <- K.table(n=n,alpha=alpha,P=P,method=method,m=m,side=side)
    all.k <- sapply(tmp,c)
    all.k <- all.k[1:min(nrow(all.k)),]
    tmp.alpha <- rep(1-alpha,length(P))
    tmp.P <- sort(rep(P,length(alpha)))
    labs <- cbind(formatC(tmp.alpha,width=5,format='f',digits=3,flag='0'),formatC(tmp.P,width=5,format='f',digits=3,flag='0'))[1:min(nrow(all.k),10),]
    ##########
    alpha.temp <- rep(NA , length(alpha)*length(n))
    for (i in 1:length(alpha)){
      alpha.temp[(length(n)*(i-1)+1) : ((length(n)*i))] <- alpha[i]
    }
    alpha.temp <- rep(alpha.temp , length(P))
    
    P.temp <- rep(NA , length(P)*length(alpha*length(n)))
    for (i in 1:length(P)){
      P.temp[(length(n)*length(alpha)*(i-1)+1) : (length(n)*length(alpha)*i)] <- P[i]
    }
    
    legend.names <- rep(NA,length(P)*length(alpha)*length(n))
    for (i in 1:length(legend.names)){
      legend.names[i] <- paste("(",paste((1-alpha.temp[i]),P.temp[i] , sep = ","),")")
    }
    
    group <- rep(NA,length(P)*length(alpha)*length(n))
    for (i in 1:(length(P)*length(alpha))) {
      group[(length(n)*(i-1)+1):(length(n)*i)] <- i
    }
    
    all.k.num <- as.numeric(as.character(as.vector(t(all.k))))
    
    data.k <- as.data.frame(cbind(rep(n , (length(alpha)*length(P))),
                                  as.vector(all.k.num),
                                  alpha.temp,
                                  P.temp,
                                  group
                                  ))
    colnames(data.k) <- c("n" , "k" , "alpha" , "P" ,"group")
    ##################
    ggplot(data = data.k , aes(x=n , y=k , group=group)) +
      scale_x_continuous(limits = c(min(n) , max(n)) , breaks = pretty_breaks()) +
      scale_y_continuous(limits = c(0 , max(all.k)) , breaks = pretty_breaks()) +
      xlab("n") + ylab("k") +
      geom_line(aes(colour = as.factor(group))) +
      geom_point(aes(colour = as.factor(group))) +
      ggtitle(paste("Normal Tolerance Interval OC Curve for k and n")) +
      scale_color_discrete(name = expression(paste("(1-",alpha," , P)")),
                           labels=unique(legend.names))
  } 
  ### Otherwise ###
  else{
    stop(paste("Check values specified for k, n, alpha, and P!", 
               "\n"))
  }
  ###### End Function ######
}



############
### Test ###
############
ggnorm.OC(k = 3, alpha = seq(0.01 , 0.05 , by=0.005),
          n = seq(10,20,2), side = 1)

ggnorm.OC(k = 3, P = seq(0.95 , 0.99 , by=0.01),
          n = seq(10,20,2), side = 1)

ggnorm.OC(k = NULL, P = c(0.90, 0.95),
        alpha=c(0.10 , 0.05 , 0.04), n = 40:60, side = 1)








