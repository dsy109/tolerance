library(tolerance)
library(ggplot2)
library(plotly)

ggplottol.hist <- function (tol.out, 
                            x,  
                            side = c("two","upper", "lower"), 
                            x.lab = NULL,
                            x.lab.size = NULL,
                            x.tick.size = NULL,
                            y.lab.size = NULL,
                            y.tick.size = NULL,
                            title.size = NULL,
                            title.position.x = NULL,
                            title.position.y = NULL,
                            bin.col = NULL,
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
  vline <- function(x = 0, color = tol.col) {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = tol.col,
                  dash = "dash",
                  width = tol.lwd)
    )
  }
  
  if (is.null(x.lab)) {
    x.lab <- "X"
  }
  if (is.null(x.lab.size)) {
    x.lab.size <- 12
  }
  if (is.null(x.tick.size)) {
    x.tick.size <- 12
  }
  if (is.null(y.lab.size)) {
    y.lab.size <- 12
  }
  if (is.null(y.tick.size)) {
    y.tick.size <- 12
  }
  if (is.null(title.size)) {
    title.size <- 12
  }
  if (is.null(title.position.x)) {
    title.position.x <- 0.5
  }
  if (is.null(title.position.y)) {
    title.position.y <- 0.95
  }
  if (is.null(bin.col)) {
    bin.col <- "#1f77b4"
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
    
    if (colnames(tol.out)[(n.c - 1)] == "1-sided.lower") {
      if (side == 'lower') {
        plot_ly(x=x ,
                type = 'histogram' , histnorm = "probability" ,
                name = 'Data' , showlegend = FALSE,
                marker = list(color = bin.col,
                              line = list(color = bin.col))) %>%
          layout(
            title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
                                      "% Lower Tolerance Limit", sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = "Density",
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size)),
            bargap = 0.01,
            shapes = list(vline(out[, (n.c - 1)]))
          )
        # ggplotly(
        #   ggplot()+
        #     geom_histogram(aes(x=x , y=..density..),
        #                    binwidth = binwidth,
        #                    bins = n.bin,
        #                    fill= bin.fill,
        #                    col= bin.col,
        #                    alpha= bin.alpha) +
        #     geom_col(width = 3) +
        #     geom_vline(xintercept = out[, n.c-1], linetype="dotted",
        #                color = tol.col, size = tol.lwd) +
        #     ggtitle(paste("One-Sided ",alpha * 100, "% / ", P * 100,
        #                   "% Lower Tolerance Limit", sep = "")) +
        #     ylab("Density") + xlab(x.lab) +
        #     scale_x_continuous(breaks = integer_breaks())+
        #     theme(plot.title = element_text(vjust = title.position.y , hjust = title.position.x ,
        #                                     size=title.size, face="bold", colour = "black"),
        #           axis.title.x = element_text(size=x.lab.size, face="bold", colour = "black"),
        #           axis.title.y = element_text(size=y.lab.size, face="bold", colour = "black"),
        #           axis.text.x = element_text(size=x.tick.size, face="bold", colour = "black"),
        #           axis.text.y = element_text(size=y.tick.size, face="bold", colour = "black"))
        # )
      } else if (side == "upper") {
        plot_ly(x=x ,
                type = 'histogram' , histnorm = "probability" ,
                name = 'Data' , showlegend = FALSE,
                marker = list(color = bin.col,
                              line = list(color = bin.col))) %>%
          layout(
            title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
                                      "% Upper Tolerance Limit", sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = "Density",
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size)),
            bargap = 0.01,
            shapes = list(vline(out[, (n.c)]))
          )
        # ggplotly(
        #   ggplot()+
        #     geom_histogram(aes(x=x , y=..density..),
        #                    binwidth = binwidth,
        #                    bins = n.bin,
        #                    fill= bin.fill,
        #                    col= bin.col,
        #                    alpha= bin.alpha) +
        #     geom_vline(xintercept = out[, n.c], linetype="dotted",
        #                color = tol.col, size = tol.lwd) +
        #     ggtitle(paste("One-Sided ",alpha * 100, "% / ", P * 100,
        #                   "% Upper Tolerance Limit", sep = "")) +
        #     ylab("Density") + xlab(x.lab) +
        #     scale_x_continuous(breaks = integer_breaks())+
        #     theme(plot.title = element_text(vjust = title.position.y , hjust = title.position.x ,
        #                                     size=title.size, face="bold", colour = "black"),
        #           axis.title.x = element_text(size=x.lab.size, face="bold", colour = "black"),
        #           axis.title.y = element_text(size=y.lab.size, face="bold", colour = "black"),
        #           axis.text.x = element_text(size=x.tick.size, face="bold", colour = "black"),
        #           axis.text.y = element_text(size=y.tick.size, face="bold", colour = "black"))
        # )
      } else if (side == "two") {
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
        plot_ly(x=x ,
                type = 'histogram' , histnorm = "probability" ,
                name = 'Data' , showlegend = FALSE,
                marker = list(color = bin.col,
                              line = list(color = bin.col))) %>%
          layout(
            title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
                                      "% Tolerance Limits", sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)),
            xaxis = list(title = x.lab,
                         tickfont = list(size = x.tick.size),
                         titlefont = list(size = x.lab.size)),
            yaxis = list(title = "Density",
                         tickfont = list(size = y.tick.size),
                         titlefont = list(size = y.lab.size)),
            bargap = 0.01,
            shapes = list(vline(out[, (n.c - 1)]),
                          vline(out[, (n.c)]))
          )
        # ggplotly(
        #   ggplot()+
        #     geom_histogram(aes(x=x , y=..density..),
        #                    binwidth = binwidth,
        #                    bins = n.bin,
        #                    fill= bin.fill,
        #                    col= bin.col,
        #                    alpha= bin.alpha) +
        #     geom_vline(xintercept = out[, n.c-1], linetype="dotted",
        #                color = tol.col, size = tol.lwd) +
        #     geom_vline(xintercept = out[, n.c], linetype="dotted",
        #                color = tol.col, size = tol.lwd) +
        #     ggtitle(paste("One-Sided ",alpha * 100, "% / ", P * 100,
        #                   "% Tolerance Limits", sep = "")) +
        #     ylab("Density") + xlab(x.lab) +
        #     scale_x_continuous(breaks = integer_breaks())+
        #     theme(plot.title = element_text(vjust = title.position.y , hjust = title.position.x ,
        #                                     size=title.size, face="bold", colour = "black"),
        #           axis.title.x = element_text(size=x.lab.size, face="bold", colour = "black"),
        #           axis.title.y = element_text(size=y.lab.size, face="bold", colour = "black"),
        #           axis.text.x = element_text(size=x.tick.size, face="bold", colour = "black"),
        #           axis.text.y = element_text(size=y.tick.size, face="bold", colour = "black"))
        # )
      }
    }
    else {
      plot_ly(x=x ,
              type = 'histogram' , histnorm = "probability" ,
              name = 'Data' , showlegend = FALSE,
              marker = list(color = bin.col,
                            line = list(color = bin.col))) %>%
        layout(
          title = list(text = paste("Two-Sided ",(1-alpha) * 100, "% / ", P * 100, 
                                    "% Tolerance Limits", sep = ""),
                       x = title.position.x,
                       y = title.position.y,
                       font = list(size=title.size)),
          xaxis = list(title = x.lab,
                       tickfont = list(size = x.tick.size),
                       titlefont = list(size = x.lab.size)),
          yaxis = list(title = "Density",
                       tickfont = list(size = y.tick.size),
                       titlefont = list(size = y.lab.size)),
          bargap = 0.01,
          shapes = list(vline(out[, (n.c - 1)]),
                        vline(out[, (n.c)]))
        )
      # ggplotly(
      #   ggplot()+
      #     geom_histogram(aes(x=x , y=..density..),
      #                    binwidth = binwidth,
      #                    bins = n.bin,
      #                    fill= bin.fill,
      #                    col= bin.col,
      #                    alpha= bin.alpha) +
      #     geom_vline(xintercept = out[, n.c-1], linetype="dotted",
      #                color = tol.col, size = tol.lwd) +
      #     geom_vline(xintercept = out[, n.c], linetype="dotted",
      #                color = tol.col, size = tol.lwd) +
      #     ggtitle(paste(alpha * 100, "% / ", P * 100,
      #                   "% Tolerance Limits", sep = "")) +
      #     ylab("Density") + xlab(x.lab) +
      #     scale_x_continuous(breaks = integer_breaks())+
      #     theme(plot.title = element_text(vjust = title.position.y , hjust = title.position.x ,
      #                                     size=title.size, face="bold", colour = "black"),
      #           axis.title.x = element_text(size=x.lab.size, face="bold", colour = "black"),
      #           axis.title.y = element_text(size=y.lab.size, face="bold", colour = "black"),
      #           axis.text.x = element_text(size=x.tick.size, face="bold", colour = "black"),
      #           axis.text.y = element_text(size=y.tick.size, face="bold", colour = "black"))
      # )
    }
  }  
}
