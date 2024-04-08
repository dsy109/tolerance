plotly_controltol <- function (tol.out , 
                               x , 
                               side = c("two","upper", "lower"),
                               x.lab = NULL,
                               x.lab.size = NULL,
                               y.lab = NULL,
                               y.lab.size = NULL,
                               x.tick.size = NULL,
                               y.tick.size = NULL,
                               x.col = NULL,
                               x.cex = NULL,
                               fit.col = NULL,
                               fit.lwd = NULL,
                               fit.line.type = c("solid","dash","dot","dashdot"),
                               tol.col = NULL,
                               tol.lwd = NULL,
                               tol.line.type = c("dash","dot","dashdot","solid"),
                               title.position.x = NULL,
                               title.position.y = NULL,
                               title.size = NULL,
                               title = NULL) {
  if (is.null(x.lab)) {
    x.lab <- 'X'
  }
  if (is.null(x.lab.size)) {
    x.lab.size <- 15
  }
  if (is.null(y.lab)) {
    y.lab <- 'Y'
  } 
  if (is.null(y.lab.size)) {
    y.lab.size <- 15
  }
  if (is.null(x.tick.size)) {
    x.tick.size <- 15
  }
  if (is.null(y.tick.size)) {
    y.tick.size <- 15
  }
  if (is.null(x.col)) {
    x.col <- "#1f77b4"
  } 
  if (is.null(x.cex)) {
    x.cex <- 6
  }
  if (is.null(fit.col)) {
    fit.col <- "#1f77b4"
  } 
  if (is.null(fit.lwd)) {
    fit.lwd <- 2
  }
  if (is.null(tol.col)) {
    tol.col <- "#d62728"
  } 
  if (is.null(tol.lwd)) {
    tol.lwd <- 2
  }
  if (is.null(title.position.x)) {
    title.position.x <- 0.5
  }
  if (is.null(title.position.y)) {
    title.position.y <- 0.95
  }
  if (is.null(title.size)) {
    title.size <- 15
  }
  ###### If X length is 1 ######
  if (length(x) == 1) {
    stop(paste("There are no plots produced for discrete distribution tolerance intervals.", 
               "\n"))
  }
  ###### If X is a dataset ######
  else {
    if (is.matrix(x)) {
      P <- as.numeric(rownames(tol.out))[1]
      alpha <- 1 - as.numeric(colnames(tol.out))[1]
    }
    else {
      side <- match.arg(side)
      alpha <- tol.out[1, 1]
      P <- tol.out[1, 2]
      out <- tol.out
      n.c <- ncol(tol.out)
      n.r <- nrow(tol.out)
      if (max(tol.out[ , n.c]) == Inf) 
      {tol.out[ , n.c] <- max(x)}
      if (min(tol.out[ , (n.c - 1)]) == -Inf) 
      {tol.out[ , n.c] <- min(x)}
    }
    
    if (colnames(tol.out)[(n.c - 1)] == "1-sided.lower") {
      if (side == "lower") {
        if (is.null(title)){
          title <- paste("One-Sided (P,",'&#947;',")=(",P,",",(1-alpha), 
                         ") Lower Tolerance Limit", sep = "")
        }
        
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(dash = fit.line.type , color = fit.col , width = fit.lwd) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = title,
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          ) %>%
          add_segments(x = 1 , xend = length(x) ,
                       y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
                       line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                       name='Lower <br>Limit' ,
                       showlegend = FALSE)
      } else if (side == "upper") {
        if (is.null(title)){
          title <- paste("One-Sided (P,",'&#947;',")=(",P,",",(1-alpha), 
                         ") Upper Tolerance Limit", sep = "")
        }
        
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(dash = fit.line.type , color = fit.col , width = fit.lwd) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = title,
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          ) %>%
          add_segments(x = 1 , xend = length(x) ,
                       y = out[, (n.c)] , yend = out[, (n.c)] ,
                       line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                       name='Upper <br>Limit' ,
                       showlegend = FALSE)
      } else if (side == "two"){
        if (is.null(title)){
          title <- paste("One-Sided (P,",'&#947;',")=(",P,",",(1-alpha), 
                         ") Tolerance Limits", sep = "")
        }
        print("NOTE: The plot reflects two 1-sided tolerance limits, NOT a 2-sided tolerance interval!")
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(dash = fit.line.type , color = fit.col , width = fit.lwd) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = title,
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          ) %>%
          add_segments(x = 1 , xend = length(x) ,
                       y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
                       line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                       name='Lower <br>Limit' ,
                       showlegend = FALSE) %>%
          add_segments(x = 1 , xend = length(x) ,
                       y = out[, (n.c)] , yend = out[, (n.c)] ,
                       line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                       name='Upper <br>Limit' ,
                       showlegend = FALSE)
      }
    }
    else {
      if (is.null(title)){
        title <- paste("Two-Sided (P,",'&#947;',")=(",P,",",(1-alpha), 
                       ") Tolerance Limits", sep = "")
      }
      plot_ly() %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                  line = list(dash = fit.line.type , color = fit.col , width = fit.lwd) ,
                  name = 'Data' , showlegend = FALSE) %>%
        layout(
          title = list(text = title,
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = x.lab,
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size)),
          yaxis = list(title = y.lab,
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size))
        ) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
                     line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                     name='Lower <br>Limit' ,
                     showlegend = FALSE) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c)] , yend = out[, (n.c)] ,
                     line = list(dash = tol.line.type , color=tol.col , width = tol.lwd) , 
                     name='Upper <br>Limit' ,
                     showlegend = FALSE)
    }
  }
}

