library(plotly)

ggplottol.anova <- function (tol.out, 
                             x, 
                             factors = NULL, 
                             tol.type = c("lower","upper"),
                             x.lab = NULL,
                             y.lab = NULL,
                             x.col = NULL,
                             x.cex = NULL,
                             tol.col = NULL,
                             tol.lwd = NULL,
                             tol.lower.col = NULL,
                             tol.lower.lwd = NULL,
                             tol.upper.col = NULL,
                             tol.upper.lwd = NULL)
{
  tol.type <- match.arg(tol.type)
  if (is.null(factors)) {
    factors <- names(tol.out)
  }
  if (is.null(x.col)) {
    x.col <- "#1f77b4"
  } 
  if (is.null(x.cex)) {
    x.cex <- 6
  }
  if (is.null(tol.col)) {
    tol.col <- "red"
  }
  if (is.null(tol.lwd)) {
    tol.lwd <- 2
  }
  if (is.null(tol.lower.col)) {
    tol.lower.col <- "red"
  }
  if (is.null(tol.lower.lwd)) {
    tol.lower.lwd <- 2
  }
  if (is.null(tol.upper.col)) {
    tol.upper.col <- "green"
  }
  if (is.null(tol.upper.lwd)) {
    tol.upper.lwd <- 2
  }
  resp <- comment(tol.out)[1]
  alpha <- 1 - as.numeric(comment(tol.out)[2])
  P <- as.numeric(comment(tol.out)[3])
  tol.out <- tol.out[match(factors,names(tol.out))]
  if (is.null(x.lab)) {
    x.lab <- names(tol.out)
  }   
  if (is.null(y.lab)) {
    y.lab <- resp
  } else {
    y.lab <- y.lab[1]
  }
  if (length(x.lab)!=length(tol.out)){
    stop(paste("Specified labels in x.lab must match the number of factors!", 
               "\n"))
  }
  temp <- NULL
  for (i in 1:length(tol.out)) {
    temp <- c(temp, unlist(tol.out[[i]][,4:5]))
  }
  temp.ind <- which(names(x) == resp)
  fact <- length(tol.out)
  if (colnames(tol.out[[1]])[4] == "1-sided.lower") {
    if (tol.type == 'lower'){
      for (i in 1:fact) {
        x.axis <- 1:nrow(tol.out[[i]])
        
        plot <- plot_ly()%>%
          add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Mean' , showlegend = FALSE) %>%
          layout(
            title = paste("One-Sided ",alpha * 100, "% / ", P * 100, 
                          "% Lower Tolerance Intervals for ",
                          names(tol.out)[[i]], sep = ""),
            xaxis = list(title = x.lab[i],
                         range = range(x.axis) + c(-0.5, 0.5),
                         ticktext = as.list(rownames(tol.out[[i]])), 
                         tickvals = as.list(x.axis),
                         tickmode = "array"),
            yaxis = list(title = y.lab,
                         range = range(c(temp, x[, temp.ind])))
          )
        
        for (j in 1:nrow(tol.out[[i]])) {
          plot <- add_segments(plot , 
                               plot , 
                               x = x.axis[j], xend = x.axis[j],
                               y = tol.out[[i]][j, 4] , yend = max(c(temp, x[, temp.ind])),
                               line = list(dash = "dash" , color=tol.upper.col , width = tol.upper.lwd) ,
                               name='Lower <br>Limit' ,
                               showlegend = FALSE)
          
        }
        print(plot) ### Has to use print to show multiple plotes.
      }
    } else{
      for (i in 1:fact) {
        x.axis <- 1:nrow(tol.out[[i]])
        
        plot <- plot_ly()%>%
          add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Mean' , showlegend = FALSE) %>%
          layout(
            title = paste("One-Sided ",alpha * 100, "% / ", P * 100, 
                          "% Upper Tolerance Intervals for ",
                          names(tol.out)[[i]], sep = ""),
            xaxis = list(title = x.lab[i],
                         range = range(x.axis) + c(-0.5, 0.5),
                         ticktext = as.list(rownames(tol.out[[i]])), 
                         tickvals = as.list(x.axis),
                         tickmode = "array"),
            yaxis = list(title = y.lab,
                         range = range(c(temp, x[, temp.ind])))
          )
        
        for (j in 1:nrow(tol.out[[i]])) {
          plot <- add_segments(plot , 
                               plot , 
                               x = x.axis[j], xend = x.axis[j],
                               y = min(c(temp, x[, temp.ind])) , yend = tol.out[[i]][j, 5],
                               line = list(dash = "dash" , color=tol.upper.col , width = tol.upper.lwd) ,
                               name='Upper <br>Limit' ,
                               showlegend = FALSE)
          
        }
        print(plot) ### Has to use print to show multiple plotes.
      }
    }      
  }  else {
    for (i in 1:length(tol.out)) {
      x.axis <- 1:nrow(tol.out[[i]])
      
      plot <- plot_ly()%>%
        add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Mean' , showlegend = FALSE) %>%
        layout(
          title = paste("Two-Sided ",alpha * 100, "% / ", P * 100, 
                        "% Tolerance Intervals for ",
                        names(tol.out)[[i]], sep = ""),
          xaxis = list(title = x.lab[i],
                       range = range(x.axis) + c(-0.5, 0.5),
                       ticktext = as.list(rownames(tol.out[[i]])), 
                       tickvals = as.list(x.axis),
                       tickmode = "array"),
          yaxis = list(title = y.lab,
                       range = range(c(temp, x[, temp.ind])))
        )
      
      for (j in 1:nrow(tol.out[[i]])) {
        plot <- add_segments(plot , 
                             x = x.axis[j], xend = x.axis[j],
                             y = tol.out[[i]][j, 4] , yend = tol.out[[i]][j, 5],
                             line = list(dash = "dash" , color=tol.col , width = tol.lwd) ,
                             name='Tolerance <br>Limits' ,
                             showlegend = FALSE
        )
      }
      print(plot) ### Has to use print to show multiple plotes.
    }
  }
}
