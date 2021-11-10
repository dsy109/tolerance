library(tolerance)
library(plotly)

plotly.plottol.anova <- function (tol.out, 
                             x, 
                             factors = NULL, 
                             side = c("two","upper", "lower"),
                             range.min = NULL,
                             range.max = NULL,
                             x.lab = NULL,
                             x.lab.size = NULL,
                             y.lab = NULL,
                             y.lab.size = NULL,
                             x.tick.size = NULL,
                             y.tick.size = NULL,
                             x.col = NULL,
                             x.cex = NULL,
                             tol.col = NULL,
                             tol.lwd = NULL,
                             tol.lower.col = NULL,
                             tol.lower.lwd = NULL,
                             tol.upper.col = NULL,
                             tol.upper.lwd = NULL,
                             title.position.x = NULL,
                             title.position.y = NULL,
                             title.size = NULL)
{
  side <- match.arg(side)
  
  if (is.null(factors)) {
    factors <- names(tol.out)
  }
  if (is.null(x.lab.size)) {
    x.lab.size <- 15
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
  
  if (is.null(range.min)) {
    range.min <- min(c(temp, x[, temp.ind]))
  }
  if (is.null(range.max)) {
    range.max <- max(c(temp, x[, temp.ind]))
  }
  
  fact <- length(tol.out)
  
  if (colnames(tol.out[[1]])[4] == "1-sided.lower") {
    if (side == 'lower'){
      for (i in 1:fact) {
        x.axis <- 1:nrow(tol.out[[i]])
        
        plot <- plot_ly()%>%
          add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Mean' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided ",alpha * 100, "% / ", P * 100, 
                                      "% Lower Tolerance Intervals for ",
                                      names(tol.out)[[i]], sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab[i],
                         range = range(x.axis) + c(-0.5, 0.5),
                         ticktext = as.list(rownames(tol.out[[i]])), 
                         tickvals = as.list(x.axis),
                         tickmode = "array",
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         range = range(c(temp, x[, temp.ind])),
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          )
        
        for (j in 1:nrow(tol.out[[i]])) {
          plot <- add_segments(plot , 
                               plot , 
                               x = x.axis[j], xend = x.axis[j],
                               y = max(tol.out[[i]][j, 4] , range.min) , yend = range.max,
                               line = list(dash = "dash" , color=tol.lower.col , width = tol.lower.lwd) ,
                               name='Lower <br>Limit' ,
                               showlegend = FALSE)%>%
            add_segments(plot , 
                         plot , 
                         x = x.axis[j]-0.025, xend = x.axis[j]+0.025,
                         y = max(tol.out[[i]][j, 4] , range.min) , 
                         yend = max(tol.out[[i]][j, 4] , range.min),
                         line = list(color=tol.lower.col , width = tol.lower.lwd) ,
                         name='Lower <br>Limit' ,
                         showlegend = FALSE)
        }
        print(plot) ### Has to use print to show multiple plotes.
      }
    } else if (side == "upper") {
      for (i in 1:fact) {
        x.axis <- 1:nrow(tol.out[[i]])
        
        plot <- plot_ly()%>%
          add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Mean' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided ",alpha * 100, "% / ", P * 100, 
                                      "% Upper Tolerance Intervals for ",
                                      names(tol.out)[[i]], sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab[i],
                         range = range(x.axis) + c(-0.5, 0.5),
                         ticktext = as.list(rownames(tol.out[[i]])), 
                         tickvals = as.list(x.axis),
                         tickmode = "array",
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         range = range(c(temp, x[, temp.ind])),
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          )
        
        for (j in 1:nrow(tol.out[[i]])) {
          plot <- add_segments(plot , 
                               plot , 
                               x = x.axis[j], xend = x.axis[j],
                               y = range.min , yend = min(tol.out[[i]][j, 5] , range.max),
                               line = list(dash = "dash" , color=tol.upper.col , width = tol.upper.lwd) ,
                               name='Upper <br>Limit' ,
                               showlegend = FALSE)%>%
            add_segments(plot , 
                         plot , 
                         x = x.axis[j]-0.025, xend = x.axis[j]+0.025,
                         y = min(tol.out[[i]][j, 5] , range.max) , 
                         yend = min(tol.out[[i]][j, 5] , range.max),
                         line = list(color=tol.upper.col , width = tol.upper.lwd) ,
                         name='Upper <br>Limit' ,
                         showlegend = FALSE)
          
        }
        print(plot) ### Has to use print to show multiple plotes.
      }
    } else if (side == "two") {
      print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      for (i in 1:fact) {
        x.axis <- 1:nrow(tol.out[[i]])
        
        plot <- plot_ly()%>%
          add_trace(x=x.axis , y=tol.out[[i]][, 1] , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Mean' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided ",alpha * 100, "% / ", P * 100, 
                                      "% Tolerance Intervals for ",
                                      names(tol.out)[[i]], sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab[i],
                         range = range(x.axis) + c(-0.5, 0.5),
                         ticktext = as.list(rownames(tol.out[[i]])), 
                         tickvals = as.list(x.axis),
                         tickmode = "array",
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = y.lab,
                         range = range(c(temp, x[, temp.ind])),
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size))
          )
        
        for (j in 1:nrow(tol.out[[i]])) {
          plot <- add_segments(plot , 
                               plot , 
                               x = x.axis[j]+0.025, xend = x.axis[j]+0.025,
                               y = range.min , yend = min(tol.out[[i]][j, 5] , range.max),
                               line = list(dash = "dash" , color=tol.upper.col , width = tol.upper.lwd) ,
                               name='Upper <br>Limit' ,
                               showlegend = FALSE) %>%
            add_segments(plot , 
                         plot , 
                         x = x.axis[j]+0.025-0.025, xend = x.axis[j]+0.025+0.025,
                         y = min(tol.out[[i]][j, 5] , range.max) , 
                         yend = min(tol.out[[i]][j, 5] , range.max),
                         line = list(color=tol.upper.col , width = tol.upper.lwd) ,
                         name='Upper <br>Limit' ,
                         showlegend = FALSE) %>%
            add_segments(plot , 
                         plot , 
                         x = x.axis[j]-0.025, xend = x.axis[j]-0.025,
                         y = max(tol.out[[i]][j, 4] , range.min) , yend = range.max,
                         line = list(dash = "dash" , color=tol.lower.col , width = tol.lower.lwd) ,
                         name='Lower <br>Limit' ,
                         showlegend = FALSE) %>%
            add_segments(plot , 
                         plot , 
                         x = x.axis[j]-0.025-0.025, xend = x.axis[j]-0.025+0.025,
                         y = max(tol.out[[i]][j, 4] , range.min) , 
                         yend = max(tol.out[[i]][j, 4] , range.min),
                         line = list(color=tol.lower.col , width = tol.lower.lwd) ,
                         name='Lower <br>Limit' ,
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
                             y = max(tol.out[[i]][j, 4] , range.min) , 
                             yend = min(tol.out[[i]][j, 5] , range.max),
                             line = list(dash = "dash" , color=tol.col , width = tol.lwd) ,
                             name='Tolerance <br>Limits' ,
                             showlegend = FALSE) %>%
          add_segments(plot , 
                       plot , 
                       x = x.axis[j]-0.025, xend = x.axis[j]+0.025,
                       y = min(tol.out[[i]][j, 5] , range.max) , 
                       yend = min(tol.out[[i]][j, 5] , range.max),
                       line = list(color=tol.col , width = tol.lwd) ,
                       name='Upper <br>Limit' ,
                       showlegend = FALSE)%>%
          add_segments(plot , 
                       plot , 
                       x = x.axis[j]-0.025, xend = x.axis[j]+0.025,
                       y = max(tol.out[[i]][j, 4] , range.min) , 
                       yend = max(tol.out[[i]][j, 4] , range.min),
                       line = list(color=tol.col , width = tol.lwd) ,
                       name='Lower <br>Limit' ,
                       showlegend = FALSE)
      }
      print(plot) ### Has to use print to show multiple plotes.
    }
  }
}

## 90%/95% 1-sided tolerance intervals for a 2-way ANOVA
## using the "warpbreaks" data.
attach(warpbreaks)
lm.out <- lm(breaks ~ wool + tension)
out.1 <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
                      P = 0.95, side = 1, method = "HE")
out.1
plotly.plottol.anova(out.1, x = warpbreaks , factors = 'wool' , x.lab = "Wool" , side="two")

## 90%/95% 2-sided tolerance intervals for a 2-way ANOVA
## using the "warpbreaks" data.
out.2 <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
                      P = 0.95, side = 2, method = "HE")
out.2
plotly.plottol.anova(out.2, x = warpbreaks , range.min = 20 , range.max = 60)
