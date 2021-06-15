library(tolerance)
library(plotly)

ggplottol.control <- function (tol.out , 
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
                               tol.col = NULL,
                               tol.lwd = NULL,
                               title.position.x = NULL,
                               title.position.y = NULL,
                               title.size = NULL) {
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
      if (side == "lower") {
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(color = x.col , width = (x.cex/3)) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided " , alpha * 100, "% / ", P * 100, 
                                      "% Lower Tolerance Limit", sep = ""),
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
                       line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                       name='Lower <br>Limit' ,
                       showlegend = FALSE)
        # ggplotly(
        #   ggplot()+
        #     geom_point(aes(x=(1:length(x)) , y=x)) +
        #     geom_line(aes(x=(1:length(x)) , y=x)) +
        #     ggtitle(label = paste(alpha * 100, "% / ", P * 100,
        #                           "% Lower Tolerance Limit", sep = "")) +
        #     theme(plot.title = element_text(hjust = 0.5)) +
        #     xlab("Index") + ylab(x.lab) +
        #     geom_hline(aes(yintercept = out[, (n.c - 1)]),
        #                linetype = "dashed",
        #                color = (2:(n.r + 1)))
        # )
      } else if (side == "upper") {
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(color = x.col , width = (x.cex/3)) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided " , alpha * 100, "% / ", P * 100, 
                                      "% Upper Tolerance Limit", sep = ""),
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
                       line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                       name='Upper <br>Limit' ,
                       showlegend = FALSE)
        # ggplotly(
        #   ggplot()+
        #     geom_point(aes(x=(1:length(x)) , y=x)) +
        #     geom_line(aes(x=(1:length(x)) , y=x)) +
        #     ggtitle(label = paste(alpha * 100, "% / ", P * 100,
        #                           "% Lower Tolerance Limit", sep = "")) +
        #     theme(plot.title = element_text(hjust = 0.5)) +
        #     xlab("Index") + ylab(x.lab) +
        #     geom_hline(aes(yintercept = out[, (n.c - 1)]),
        #                linetype = "dashed",
        #                color = (2:(n.r + 1)))
        # )
      } else if (side == "two"){
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
        plot_ly() %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                    line = list(color = x.col , width = (x.cex/3)) , 
                    name = 'Data' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("One-Sided ", alpha * 100, "% / ", P * 100, 
                                      "% Tolerance Limits", sep = ""),
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
                       line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                       name='Lower <br>Limit' ,
                       showlegend = FALSE) %>%
          add_segments(x = 1 , xend = length(x) ,
                       y = out[, (n.c)] , yend = out[, (n.c)] ,
                       line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                       name='Upper <br>Limit' ,
                       showlegend = FALSE)
        # ggplotly(
        #   ggplot()+
        #     geom_point(aes(x=(1:length(x)) , y=x)) +
        #     geom_line(aes(x=(1:length(x)) , y=x)) +
        #     ggtitle(label = paste(alpha * 100, "% / ", P * 100,
        #                           "% Tolerance Limits", sep = "")) +
        #     theme(plot.title = element_text(hjust = 0.5)) +
        #     xlab("Index") + ylab(x.lab) +
        #     geom_hline(aes(yintercept = out[, (n.c - 1)]),
        #                linetype = "dashed",
        #                color = (2:(n.r + 1))) +
        #     geom_hline(aes(yintercept = out[, (n.c)]),
        #                linetype = "dashed",
        #                color = (2:(n.r + 1)))
        # )
      }
    }
    else {
      plot_ly() %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=(1:length(x)) , y=x , type = 'scatter' , mode = 'lines' ,
                  line = list(color = x.col , width = (x.cex/3)) , 
                  name = 'Data' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste("Two-Sided " , alpha * 100, "% / ", P * 100, 
                                    "% Tolerance Limits", sep = ""),
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
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Lower <br>Limit' ,
                     showlegend = FALSE) %>%
        add_segments(x = 1 , xend = length(x) ,
                     y = out[, (n.c)] , yend = out[, (n.c)] ,
                     line = list(dash = "dash" , color=tol.col , width = tol.lwd) , 
                     name='Upper <br>Limit' ,
                     showlegend = FALSE)
      # ggplotly(
      #   ggplot()+
      #     geom_point(aes(x=(1:length(x)) , y=x)) +
      #     geom_line(aes(x=(1:length(x)) , y=x)) +
      #     ggtitle(label = paste(alpha * 100, "% / ", P * 100,
      #                           "% Tolerance Limits", sep = "")) +
      #     theme(plot.title = element_text(hjust = 0.5)) +
      #     xlab("Index") + ylab(x.lab) +
      #     geom_hline(aes(yintercept = out[, (n.c - 1)]),
      #                linetype = "dashed",
      #                color = (2:(n.r + 1))) +
      #     geom_hline(aes(yintercept = out[, (n.c)]),
      #                linetype = "dashed",
      #                color = (2:(n.r + 1)))
      # )
    }
  }
}
