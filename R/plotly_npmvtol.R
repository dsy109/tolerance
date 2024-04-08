plotly_npmvtol <- function(tol.out, 
                           x, 
                           var.names = NULL, 
                           title = NULL,
                           x.col = "#4298B5",
                           x.cex = 6,
                           x.shape = "dot",
                           outlier.col = "#A6192E",
                           outlier.cex = 8,
                           outlier.shape = "triangle-up",
                           tol.col = "#D1DDE6",
                           tol.opacity = 0.4,
                           x.lab.size = 12,
                           x.tick.size = 12,
                           y.lab.size = 12,
                           y.tick.size = 12,
                           title.position.x = 0.5,
                           title.position.y = 0.98,
                           title.size = 12,
                           show.bound = TRUE, 
                           bound.type = c("dash", "dot", "solid", "longdash", "dashdot","longdashdot"), 
                           bound.col = "#000000",
                           bound.lwd = 1
                           ){
  # Horizontal Line #
  hline <- function(y , color = "#000000" , dash = "dot" , width = 1) {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color , dash = dash , width = width)
    )
  }
  # Verticle Line #
  vline <- function(x = 0, color = "#000000" , dash = "dot" , width = 1) {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color , dash = dash , width = width)
    )
  }
  
  p <- nrow(tol.out)
  x <- data.frame(x)
  if(sum(colnames(x)%in%rownames(tol.out))!=p) {
    stop(paste("All of the row names in 'tol.out' must match to a variable name in 'x'.", 
               "\n"))
  }
  
  v <- rownames(tol.out)
  temp <- sapply(1:p, function(i) !((x[v[i]]>=tol.out[v[i],1])&(x[v[i]]<=tol.out[v[i],2])))
  temp <- as.factor(as.numeric(apply(temp,1,any)))
  df <- data.frame(x, outlier=temp)
  if(is.null(var.names)) {var.names <- v}
  p.c <- combn(length(var.names),2) ### Combination of Variables ###
  p.n <- ncol(p.c) ### Number of Combination ###
  
  if (is.null(title)){
    title <- "Nonparametric Multivariate Tolerance Region"
  }
  if (length(title) == 1){
    title <- rep(title , p.n)
  }
  if(length(title) != p.n) {
    stop(paste("Either 1 or",p.n,"titles need to be specified.", 
               "\n"))
  }
  
  if (show.bound){
    for (i in 1:p.n){
      data <- df[,c(v[p.c[,i]],"outlier")]
      x.min <- min(data[,1])
      x.max <- max(data[,1])
      y.min <- min(data[,2])
      y.max <- max(data[,2])
      
      if (tol.out[v[p.c[,i]][1],1] == -Inf){
        x0 <- x.min-5
      } else {x0 <- tol.out[v[p.c[,i]][1],1]}
      if (tol.out[v[p.c[,i]][1],2] == Inf){
        x1 <- x.max+5
      } else {x1 <- tol.out[v[p.c[,i]][1],2]}
      if (tol.out[v[p.c[,i]][2],1] == -Inf){
        y0 <- y.min-5
      } else {y0 <- tol.out[v[p.c[,i]][2],1]}
      if (tol.out[v[p.c[,i]][2],2] == Inf){
        y1 <- x.max+5
      } else {y1 <- tol.out[v[p.c[,i]][2],2]}
      
      plot <- plot_ly()%>%
        add_trace(x = data[which(data[,3] == 0),1],
                  y = data[which(data[,3] == 0),2],
                  type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col ,  size = x.cex , symbol = x.shape),
                  name = "Data" , showlegend = FALSE) %>%
        add_trace(x = data[which(data[,3] == 1),1],
                  y = data[which(data[,3] == 1),2],
                  type = 'scatter' , mode = 'markers' ,
                  marker = list(color = outlier.col ,  size = outlier.cex , symbol = outlier.shape),
                  name = "Outlier" , showlegend = FALSE)%>%
        layout(
          shapes = list(hline(y = y0 , dash = bound.type , width = bound.lwd , color = bound.col),
                        hline(y = y1 , dash = bound.type , width = bound.lwd , color = bound.col),
                        vline(x = x0 , dash = bound.type , width = bound.lwd , color = bound.col),
                        vline(x = x1 , dash = bound.type , width = bound.lwd , color = bound.col),
                        list(type = "rect" , 
                             fillcolor = tol.col,
                             opacity = tol.opacity,
                             line = list(color = tol.col),
                             x0 = x0, 
                             x1 = x1, 
                             y0 = y0, 
                             y1 = y1)),
          title = list(text = title[i],
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = var.names[p.c[,i]][1],
                       range = c((x.min-0.5) , (x.max+0.5)),
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size),
                       zeroline = FALSE),
          yaxis = list(title = var.names[p.c[,i]][2],
                       range = c((y.min-0.5) , (y.max+0.5)),
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size),
                       zeroline = FALSE)
        )
      print(plot)
    }
  }
  
  if (!show.bound){
    for (i in 1:p.n){
      data <- df[,c(v[p.c[,i]],"outlier")]
      x.min <- min(data[,1])
      x.max <- max(data[,1])
      y.min <- min(data[,2])
      y.max <- max(data[,2])
      
      if (tol.out[v[p.c[,i]][1],1] == -Inf){
        x0 <- x.min-5
      } else {x0 <- tol.out[v[p.c[,i]][1],1]}
      if (tol.out[v[p.c[,i]][1],2] == Inf){
        x1 <- x.max+5
      } else {x1 <- tol.out[v[p.c[,i]][1],2]}
      if (tol.out[v[p.c[,i]][2],1] == -Inf){
        y0 <- y.min-5
      } else {y0 <- tol.out[v[p.c[,i]][2],1]}
      if (tol.out[v[p.c[,i]][2],2] == Inf){
        y1 <- x.max+5
      } else {y1 <- tol.out[v[p.c[,i]][2],2]}
      
      plot <- plot_ly()%>%
        add_trace(x = data[which(data[,3] == 0),1],
                  y = data[which(data[,3] == 0),2],
                  type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col ,  size = x.cex , symbol = x.shape),
                  name = "Data" , showlegend = FALSE) %>%
        add_trace(x = data[which(data[,3] == 1),1],
                  y = data[which(data[,3] == 1),2],
                  type = 'scatter' , mode = 'markers' ,
                  marker = list(color = outlier.col ,  size = outlier.cex , symbol = outlier.shape),
                  name = "Outlier" , showlegend = FALSE)%>%
        layout(
          shapes = list(list(type = "rect" , 
                             fillcolor = tol.col,
                             opacity = tol.opacity,
                             line = list(color = tol.col),
                             x0 = x0, 
                             x1 = x1, 
                             y0 = y0, 
                             y1 = y1)),
          title = list(text = title[i],
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = var.names[p.c[,i]][1],
                       range = c((x.min-0.5) , (x.max+0.5)),
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size),
                       zeroline = FALSE),
          yaxis = list(title = var.names[p.c[,i]][2],
                       range = c((y.min-0.5) , (y.max+0.5)),
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size),
                       zeroline = FALSE)
        )
      print(plot)
    }
  }
  
}

