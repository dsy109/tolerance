library(tolerance)
library(plotly)

ggplottol.reg <- function (tol.out,
                           x,
                           y = NULL,
                           y.hat = NULL,
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
  
  side <- match.arg(side)
  
  if (length(x) == 1) {
    stop(paste("There are no plots produced for discrete distribution tolerance intervals.", 
               "\n"))
  }
  if (class(tol.out)[1] == "list") {
    y.lim = range(sapply(1:length(tol.out), function(i) tol.out[[i]][, 
                                                                     c(ncol(tol.out[[i]]) - 1, ncol(tol.out[[i]]))]))
    temp.tol <- tol.out
    tol.out <- tol.out[[1]]
  }
  else {
    temp.tol <- NULL
    y.lim <- range(tol.out[, c(ncol(tol.out) - 1, ncol(tol.out))])
  }
  
  if (is.matrix(x) & is.null(y)) {
    P <- as.numeric(rownames(tol.out))[1]
    alpha <- 1 - as.numeric(colnames(tol.out))[1]
  }
  else {
    alpha <- 1 - tol.out[1, 1]
    P <- tol.out[1, 2]
    out <- tol.out
    n.c <- ncol(tol.out)
    n.r <- nrow(tol.out)
    if (max(tol.out[, n.c]) == Inf) 
      tol.out[, n.c] <- max(x)
    if (min(tol.out[, (n.c - 1)]) == -Inf) 
      tol.out[, n.c] <- min(x)
  }
  ##########################################
  if (colnames(out)[3] == "y.hat") {
    out1 <- cbind(x, y)
    out1 <- out1[order(out1[, 2]), ]
    out1 <- cbind(out1[, 1], out[1:length(x), ])
    out1 <- out1[order(out1[, 1]), ]
    
    if (side == 'upper') {
      plot <- plot_ly() %>%
        add_trace(x=x , y=y , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,4] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = fit.col , size = fit.lwd) , 
                  name = 'Fitted Line' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,7] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Upper Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
    } else if (side == 'lower') {
      plot <- plot_ly() %>%
        add_trace(x=x , y=y , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,4] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = fit.col , size = fit.lwd) , 
                  name = 'Fitted Line' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,6] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Lower Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
    } else if (side == "two") {
      if (colnames(out1)[6] == "1-sided.lower") 
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      
      plot <- plot_ly() %>%
        add_trace(x=x , y=y , type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,4] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = fit.col , size = fit.lwd) , 
                  name = 'Fitted Line' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,6] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Lower Limit' , showlegend = FALSE) %>%
        add_trace(x=out1[,1] , y=out1[,7] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Upper Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
    }
    print(plot)
  }
  ################################################################
  if (is.null(temp.tol) == "FALSE" | is.null(y.hat) == "FALSE") {
    if (class(temp.tol)[1] != "list") {
      temp.tol <- list(tol.out)
    }
    out1 <- temp.tol
    len <- length(out1)
    xy.out <- lapply(1:len, function(i) out1[[i]][,3:7])
    xy.out <- lapply(1:len, function(i) xy.out[[i]][order(xy.out[[i]][,1]), ])
    
    plot <- plot_ly() %>%
      add_trace(x=x , y=y , type = 'scatter' , mode = 'markers' ,
                marker = list(color = x.col , size = x.cex) , 
                name = 'Data' , showlegend = FALSE) %>%
      add_trace(x=xy.out[[1]][, 1] , y=xy.out[[1]][, 3] , type = 'scatter' , mode = 'lines' ,
                line = list(color = fit.col , size = fit.lwd) , 
                name = 'Fitted Line' , showlegend = FALSE)
    
    if (side == "upper") {
      for (i in 1:len) {
        plot <- add_trace(plot,
                          plot,
                          x=xy.out[[i]][, 1] , y=xy.out[[i]][, 5] , type = 'scatter' , mode = 'lines' ,
                          line = list(color = i+1 , size = fit.lwd) , 
                          name = 'Upper Limit' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
          )
      }
      print(plot)
    }
    else if (side == "lower") {
      for (i in 1:len) {
        plot <- add_trace(plot,
                          plot,
                          x=xy.out[[i]][, 1] , y=xy.out[[i]][, 4] , type = 'scatter' , mode = 'lines' ,
                          line = list(color = i+1 , size = fit.lwd) , 
                          name = 'Lower Limit' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
          )
      }
      print(plot)
    }
    else if (side == "two") {
      if (colnames(out1[[1]])[5] == "1-sided.lower") 
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      for (i in 1:len) {
        plot <- add_trace(plot,
                          plot,
                          x=xy.out[[i]][, 1] , y=xy.out[[i]][, 4] , type = 'scatter' , mode = 'lines' ,
                          line = list(color = i+1 , size = fit.lwd) , 
                          name = 'Lower Limit' , showlegend = FALSE) %>%
          add_trace(plot,
                    plot,
                    x=xy.out[[i]][, 1] , y=xy.out[[i]][, 5] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = i+1 , size = fit.lwd) , 
                    name = 'Upper Limit' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
          )
      }
      print(plot)
    }
  }
  ########################################
  if (colnames(out)[3] == "y") {
    y.lim <- range(out[, 5:6])
    if (class(x)[1] == "numeric") 
      x <- matrix(x, ncol = 1)
    if (sum(x[, 1] == 1) != nrow(x)) 
      print("NOTE: A regression through the origin is fitted!")
    if (sum(x[, 1] == 1) == nrow(x)) {
      XXX <- x[, 2]
      reg.out <- lm(y ~ x - 1)
      temp.x <- seq(min(x[, 2]), max(x[, 2]), length.out = 1000)
    }
    else {
      XXX <- x[,1]
      reg.out <- lm(y ~ x - 1)
      temp.x <- seq(min(x[, 1]), max(x[, 1]), length.out = 1000)
    }
    
    b <- reg.out$coef
    out.2 <- out[order(out[, 4]), ]
    out.temp <- apply(is.na(out.2), 1, sum)
    out.2 <- out.2[out.temp == 0, ]
    if (sum(x[, 1] == 1) == nrow(x)) {
      temp.x = cbind(1, sapply(1:(ncol(x) - 1), function(i) temp.x^i))
      poly.x = cbind(temp.x[, 2], apply(t(b * t(temp.x)), 
                                        1, sum))
      n.x <- ncol(poly.x)
    }
    else {
      temp.x <- as.matrix(sapply(1:ncol(x), function(i) temp.x^i), 
                          ncol = ncol(x))
      poly.x <- cbind(temp.x[, 1], apply(t(b * t(temp.x)), 
                                         1, sum))
      n.x <- ncol(poly.x)
    }
    
    plot <- plot_ly() %>%
      add_trace(x=XXX , y=y , type = 'scatter' , mode = 'markers' ,
                marker = list(color = x.col , size = x.cex) , 
                name = 'Data' , showlegend = FALSE) %>%
      add_trace(x=poly.x[,1] , y=poly.x[,2] , type = 'scatter' , mode = 'lines' ,
                line = list(color = fit.col , size = fit.lwd) , 
                name = 'Fitted Line' , showlegend = FALSE)
    
    x.temp <- cbind(XXX, fitted(reg.out))
    x.temp <- x.temp[order(x.temp[, 2]), ]
    temp <- cbind(x.temp[, 1], out.2[, 4:6])
    temp <- temp[order(temp[, 1]), ]
    if (side == "upper") {
      plot <- add_trace(plot,
                        plot,
                        x=temp[, 1] , y=temp[, 4] , type = 'scatter' , mode = 'lines' ,
                        line = list(color = tol.col , size = tol.lwd) , 
                        name = 'Upper Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
      print(plot)
    }
    else if (side == "lower") {
      plot <- add_trace(plot,
                        plot,
                        x=temp[, 1] , y=temp[, 3] , type = 'scatter' , mode = 'lines' ,
                        line = list(color = tol.col , size = tol.lwd) , 
                        name = 'Lower Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
      print(plot)
    }
    else if (side == "two") {
      if (colnames(tol.out)[5] == "1-sided.lower") 
        print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
      
      plot <- add_trace(plot,
                        plot,
                        x=temp[, 1] , y=temp[, 3] , type = 'scatter' , mode = 'lines' ,
                        line = list(color = tol.col , size = tol.lwd) , 
                        name = 'Lower Limit' , showlegend = FALSE) %>%
        add_trace(plot,
                  plot,
                  x=temp[, 1] , y=temp[, 4] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Upper Limit' , showlegend = FALSE) %>%
        layout(
          title = list(text = paste(alpha * 100, "% / ", P * 100, 
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
        )
      print(plot)
    }
  }
}