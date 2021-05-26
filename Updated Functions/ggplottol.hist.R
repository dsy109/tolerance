library(tolerance)
library(ggplot2)
library(plotly)

ggplottol.hist <- function (tol.out, 
                            x,  
                            side = c("two","upper", "lower"), 
                            freq = TRUE,
                            x.lab = NULL,
                            n.xlab = NULL,
                            n.bin = NULL,
                            binwidth = NULL,
                            bin.fill = NULL,
                            bin.col = NULL,
                            bin.alpha = NULL,
                            tol.col = NULL,
                            tol.lwd = NULL) 
{
  if (is.null(n.xlab)) {
    n.xlab <- 10
  }
  integer_breaks <- function(n = n.xlab) {
    fxn <- function(x) {
      breaks <- pretty(x, n)
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  if (is.null(x.lab)) {
    x.lab <- "X"
  }
  if (!is.null(n.bin)) {
    n.bin <- n.bin
  } 
  if (!is.null(binwidth)) {
    binwidth <- binwidth
  }
  if (is.null(bin.fill)) {
    bin.fill <- "#1f77b4"
  } 
  if (is.null(bin.col)) {
    bin.col <- "#e9ecef"
  } 
  if (is.null(bin.alpha)) {
    bin.alpha <- 1
  }
  if (is.null(tol.col)) {
    tol.col <- "#d62728"
  } 
  if (is.null(tol.lwd)) {
    tol.lwd <- 1
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
      # plot_ly(x=x , 
      #         type = 'histogram' , histnorm = "probability" ,
      #         # nbinsx = 20 , 
      #         name = 'Data' , showlegend = FALSE) %>%
      #   layout(
      #     title = paste(a * 100, "% / ", P * 100, 
      #                   "% Lower Tolerance Limit", sep = ""),
      #     xaxis = list(title = x.lab),
      #     yaxis = list(title = "Density"),
      #     bargap = 0.01
      #   ) %>%
      #   add_segments(x = out[, (n.c - 1)] , xend = out[, (n.c - 1)] ,
      #                y = 0 , yend = max(density(x , bw=0.035)$y) ,
      #                line = list(dash = "dash" , color="red") , name='Lower <br>Limit',
      #                showlegend = FALSE)
      ggplotly(
        ggplot()+
          geom_histogram(aes(x=x , y=..density..),
                         binwidth = binwidth,
                         bins = n.bin,
                         fill= bin.fill,
                         col= bin.col,
                         alpha= bin.alpha) +
          geom_col(width = 3) +
          geom_vline(xintercept = out[, n.c-1], linetype="dotted",
                     color = tol.col, size = tol.lwd) +
          ggtitle(paste(alpha * 100, "% / ", P * 100,
                        "% Lower Tolerance Limit", sep = "")) +
          ylab("Density") + xlab(x.lab) +
          scale_x_continuous(breaks = integer_breaks())+
          theme(plot.title = element_text(vjust = 0 , hjust = 0.5 ,
                                          size=12, face="bold", colour = "black"),
                axis.title.x = element_text(size=12, face="bold", colour = "black"),
                axis.title.y = element_text(size=12, face="bold", colour = "black"),
                axis.text.x = element_text(size=12, face="bold", colour = "black"),
                axis.text.y = element_text(size=12, face="bold", colour = "black"))
      )
    } else if (side == "upper") {
      # plot_ly() %>%
      #   add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
      #             marker = list(color = x.col , size = x.cex) , 
      #             name = 'Data' , showlegend = FALSE) %>%
      #   add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
      #             line = list(color = x.col , width = (x.cex/3)) , 
      #             name = 'Data' , showlegend = FALSE) %>%
      #   layout(
      #     title = paste(alpha * 100, "% / ", P * 100, 
      #                   "% Upper Tolerance Limit", sep = ""),
      #     xaxis = list(title = 'Index'),
      #     yaxis = list(title = x.lab)
      #   ) %>%
      #   add_segments(x = 1 , xend = length(x) ,
      #                y = out[, (n.c)] , yend = out[, (n.c)] ,
      #                line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
      #                name='Tolerance <br>Limit' ,
      #                showlegend = FALSE)
      ggplotly(
        ggplot()+
          geom_histogram(aes(x=x , y=..density..),
                         binwidth = binwidth,
                         bins = n.bin,
                         fill= bin.fill,
                         col= bin.col,
                         alpha= bin.alpha) +
          geom_vline(xintercept = out[, n.c], linetype="dotted",
                     color = tol.col, size = tol.lwd) +
          ggtitle(paste(alpha * 100, "% / ", P * 100,
                        "% Upper Tolerance Limit", sep = "")) +
          ylab("Density") + xlab(x.lab) +
          scale_x_continuous(breaks = integer_breaks())+
          theme(plot.title = element_text(vjust = 0 , hjust = 0.5 ,
                                          size=12, face="bold", colour = "black"),
                axis.title.x = element_text(size=12, face="bold", colour = "black"),
                axis.title.y = element_text(size=12, face="bold", colour = "black"),
                axis.text.x = element_text(size=12, face="bold", colour = "black"),
                axis.text.y = element_text(size=12, face="bold", colour = "black"))
      )
    } else {
      if (colnames(tol.out)[(n.c - 1)] == "1-sided.lower") 
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      # plot_ly() %>%
      #   add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
      #             marker = list(color = x.col , size = x.cex) , 
      #             name = 'Data' , showlegend = FALSE) %>%
      #   add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
      #             line = list(color = x.col , width = (x.cex/3)) , 
      #             name = 'Data' , showlegend = FALSE) %>%
      #   layout(
      #     title = paste(alpha * 100, "% / ", P * 100, 
      #                   "% Tolerance Limits", sep = ""),
      #     xaxis = list(title = 'Index'),
      #     yaxis = list(title = x.lab)
      #   ) %>%
      #   add_segments(x = 1 , xend = length(x) ,
      #                y = out[, (n.c - 1)] , yend = out[, (n.c - 1)] ,
      #                line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
      #                name='Lower <br>Limit' ,
      #                showlegend = FALSE) %>%
      #   add_segments(x = 1 , xend = length(x) ,
      #                y = out[, (n.c)] , yend = out[, (n.c)] ,
      #                line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
      #                name='Upper <br>Limit' ,
      #                showlegend = FALSE)
      ggplotly(
        ggplot()+
          geom_histogram(aes(x=x , y=..density..),
                         binwidth = binwidth,
                         bins = n.bin,
                         fill= bin.fill,
                         col= bin.col,
                         alpha= bin.alpha) +
          geom_vline(xintercept = out[, n.c-1], linetype="dotted",
                     color = tol.col, size = tol.lwd) +
          geom_vline(xintercept = out[, n.c], linetype="dotted",
                     color = tol.col, size = tol.lwd) +
          ggtitle(paste(alpha * 100, "% / ", P * 100,
                        "% Tolerance Limits", sep = "")) +
          ylab("Density") + xlab(x.lab) +
          scale_x_continuous(breaks = integer_breaks())+
          theme(plot.title = element_text(vjust = 0 , hjust = 0.5 ,
                                          size=12, face="bold", colour = "black"),
                axis.title.x = element_text(size=12, face="bold", colour = "black"),
                axis.title.y = element_text(size=12, face="bold", colour = "black"),
                axis.text.x = element_text(size=12, face="bold", colour = "black"),
                axis.text.y = element_text(size=12, face="bold", colour = "black"))
      )
    }
  }  
}
