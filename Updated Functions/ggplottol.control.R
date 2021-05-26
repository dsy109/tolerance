library(tolerance)
library(ggplot2)
library(plotly)

ggplottol.control <- function (tol.out , 
                               x , 
                               side = c("two","upper", "lower"),
                               x.lab = NULL , 
                               x.col = NULL ,
                               x.cex = NULL ,
                               tol.col = NULL ,
                               tol.lwd = NULL) {
  if (is.null(x.lab)) {
    x.lab <- "X"
  }
  if (is.null(x.col)) {
    x.col <- "#1f77b4"
  } 
  if (is.null(x.cex)) {
    x.cex <- 6
  } 
  if (is.null(tol.col)) {
    tol.col <- "#d62728"
  } 
  if (is.null(tol.lwd)) {
    tol.lwd <- 2
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
      alpha <- 1 - tol.out[1, 1]
      P <- tol.out[1, 2]
      out <- tol.out
      n.c <- ncol(tol.out)
      n.r <- nrow(tol.out)
      if (max(tol.out[ , n.c]) == Inf) 
      {tol.out[ , n.c] <- max(x)}
      if (min(tol.out[ , (n.c - 1)]) == -Inf) 
      {tol.out[ , n.c] <- min(x)}
    }
    
    if (side == "lower") {
      plot_ly() %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                  line = list(color = x.col , width = (x.cex/3)) , 
                  name = 'Data' , showlegend = FALSE) %>%
        layout(
          title = paste(alpha * 100, "% / ", P * 100, 
                        "% Lower Tolerance Limit", sep = ""),
          xaxis = list(title = 'Index'),
          yaxis = list(title = x.lab)
        ) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Tolerance <br>Limit' ,
                     showlegend = FALSE)
    } else if (side == "upper") {
      plot_ly() %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                  line = list(color = x.col , width = (x.cex/3)) , 
                  name = 'Data' , showlegend = FALSE) %>%
        layout(
          title = paste(alpha * 100, "% / ", P * 100, 
                        "% Upper Tolerance Limit", sep = ""),
          xaxis = list(title = 'Index'),
          yaxis = list(title = x.lab)
        ) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c)] , yend = out[, (n.c)] ,
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Tolerance <br>Limit' ,
                     showlegend = FALSE)
    } else {
      if (colnames(tol.out)[(n.c - 1)] == "1-sided.lower") 
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      plot_ly() %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                  line = list(color = x.col , width = (x.cex/3)) , 
                  name = 'Data' , showlegend = FALSE) %>%
        layout(
          title = paste(alpha * 100, "% / ", P * 100, 
                        "% Tolerance Limits", sep = ""),
          xaxis = list(title = 'Index'),
          yaxis = list(title = x.lab)
        ) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Lower <br>Limit' ,
                     showlegend = FALSE) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c)] , yend = out[, (n.c)] ,
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Upper <br>Limit' ,
                     showlegend = FALSE)
    }
  }
}
