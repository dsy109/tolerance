plotly_normOC <- function(k = NULL, 
                          alpha = NULL, 
                          P = NULL, 
                          n, 
                          side = 1, 
                          method = c("HE", "HE2", "WBE", "ELL", "KM", "EXACT", "OCT"), 
                          m = 50,
                          range.min = NULL,
                          range.max = NULL,
                          x.lab.size = NULL,
                          y.lab.size = NULL,
                          x.tick.size = NULL,
                          y.tick.size = NULL,
                          title = NULL,
                          title.size = NULL,
                          title.position.x = NULL,
                          title.position.y = NULL,
                          legend.size = NULL,
                          x.cex = NULL,
                          line.width = NULL,
                          line.type = c("solid","dash","dot","dashdot")) {
  if (is.null(x.lab.size)){
    x.lab.size <- 15
  }
  if (is.null(y.lab.size)){
    y.lab.size <- 15
  }
  if (is.null(x.tick.size)){
    x.tick.size <- 15
  }
  if (is.null(y.tick.size)){
    y.tick.size <- 15
  }
  if (is.null(title.size)){
    title.size <- 15
  }
  if (is.null(title.position.x)){
    title.position.x <- 0.5
  }
  if (is.null(title.position.y)){
    title.position.y <- 0.95
  }
  if (is.null(legend.size)){
    legend.size <- 15
  }
  if (is.null(x.cex)){
    x.cex <- 6
  }
  if (is.null(line.width)){
    line.width <- 2
  }
  
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
    if (is.null(range.min)){
      range.min <- min(all.P)-0.01
    }
    if (is.null(range.max)){
      range.max <- max(all.P)+0.01
    }
    colnames(data.P) <- c("n","P","alpha")
    tmp.obj <- paste("(k=",k,")",sep="")
    
    if (is.null(title)){
      title <- paste("Normal Tolerance Interval OC Curve for P (k=",k,")" , 
                     sep="")
    }
    
    plot <- plot_ly()
    for (i in 1:ncol(all.P)){
      plot <- add_trace(plot,
                        plot,
                        x=n , y=all.P[,i] , type = 'scatter' , mode = 'lines+markers',
                        marker = list(size = x.cex),
                        line = list(width = line.width , dash=line.type),
                        name = 1-alpha[i] , showlegend = TRUE) %>%
        layout(
          legend = list(title=list(text=paste("(1-",'&#945;',")",sep=""),
                                   font=list(size=legend.size)),
                        font = list(size=legend.size)),
          ### use html decimal code for greek letters. ###
          title = list(text = title,
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = "n",
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size)),
          yaxis = list(title = "P",
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size),
                       range = c(range.min , range.max))
        )
    }
    print(plot)
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
    if (is.null(range.min)){
      range.min <- min(all.alpha)-0.01
    }
    if (is.null(range.max)){
      range.max <- max(all.alpha)+0.01
    }
    colnames(data.alpha) <- c("n","alpha","P")
    tmp.obj <- paste("(k=",k,")",sep="")
    
    if (is.null(title)){
      title <- paste("Normal Tolerance Interval OC Curve for (1-",'&#945;',") (k=",k,")",
                     sep="")
    }
    
    plot <- plot_ly()
    for (i in 1:ncol(all.alpha)){
      plot <- add_trace(plot,
                        plot,
                        x=n , y=all.alpha[,i] , type = 'scatter' , mode = 'lines+markers',
                        marker = list(size = x.cex),
                        line = list(width = line.width , dash = line.type),
                        name = P[i] , showlegend = TRUE) %>%
        layout(
          legend = list(title=list(text=paste("P",sep=""),
                                   font=list(size=legend.size)),
                        font = list(size=legend.size)),
          ### use html decimal code for greek letters. ###
          title = list(text = title,
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = "n",
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size)),
          yaxis = list(title=list(text=paste("(1-",'&#945;',")",sep="")),
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size),
                       range = c(range.min , range.max))
        )
    }
    print(plot)
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
    #############
    if (is.null(range.min)){
      range.min <- min(all.k.num)-0.1
    }
    if (is.null(range.max)){
      range.max <- max(all.k.num)+0.1
    }
    if (is.null(title)){
      title <- paste("Normal Tolerance Interval OC Curve for k and n",
                     sep="")
    }
    
    plot <- plot_ly()
    for (i in 1:length((unique(group)))){
      plot <- add_trace(plot,
                        plot,
                        x=data.k$n[(length(n)*(i-1)+1):(length(n)*i)] , 
                        y=data.k$k[(length(n)*(i-1)+1):(length(n)*i)] , 
                        type = 'scatter' , mode = 'lines+markers',
                        marker = list(size = x.cex),
                        line = list(width = line.width , dash = line.type),
                        name = paste("(",1-data.k$alpha[length(n)*(i-1)+1],",",
                                     data.k$P[length(n)*(i-1)+1],")",sep="") , showlegend = TRUE) %>%
        layout(
          legend = list(title=list(text=paste("(1-",'&#945;',",P)",sep=""),
                                   font=list(size=legend.size)),
                        font = list(size=legend.size)),
          ### use html decimal code for greek letters. ###
          title = list(text = title,
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = "n",
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size)),
          yaxis = list(title=list(text=paste("k",sep="")),
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size),
                       range = c(range.min , range.max))
        )
    }
    print(plot)
  } 
  ### Otherwise ###
  else{
    stop(paste("Check values specified for k, n, alpha, and P!", 
               "\n"))
  }
}

