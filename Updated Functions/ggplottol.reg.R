ggplottol.reg <- function (tol.out,
                           x,
                           x.new = NULL,
                           y,
                           side = c("two","upper", "lower"),
                           x.lab = NULL,
                           x.lab.size = NULL,
                           y.lab = NULL,
                           y.lab.size = NULL,
                           z.lab = NULL,
                           z.lab.size = NULL,
                           x.tick.size = NULL,
                           y.tick.size = NULL,
                           z.tick.size = NULL,
                           x.col = NULL,
                           x.cex = NULL,
                           fit.col = NULL,
                           fit.lwd = NULL,
                           fit.opacity = NULL,
                           tol.col = NULL,
                           tol.lwd = NULL,
                           tol.opacity = NULL,
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
  if (is.null(z.lab)) {
    z.lab <- 'Z'
  } 
  if (is.null(z.lab.size)) {
    z.lab.size <- 15
  }
  if (is.null(x.tick.size)) {
    x.tick.size <- 15
  }
  if (is.null(y.tick.size)) {
    y.tick.size <- 15
  }
  if (is.null(z.tick.size)) {
    z.tick.size <- 15
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
  if (is.null(fit.opacity)) {
    fit.opacity <- 0.5
  }
  if (is.null(tol.col)) {
    tol.col <- "#d62728"
  } 
  if (is.null(tol.lwd)) {
    tol.lwd <- 2
  }
  if (is.null(tol.opacity)) {
    tol.opacity <- 0.5
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
  
  xy.data.original <- cbind(x,y)
  
  if (dim(xy.data.original)[2] == 2){
    if (is.null(x.new)){
      tol.fit.order <- cbind(x,tol.out$fit[match(y,tol.out$fit$y),])[order(x),]
    } else if (!is.null(x.new)) {
      if (length(c(x , x.new)) > dim(tol.out$fit)[1]) {
        stop("No tolerance limits generated for new data. Therefore, No plot generated.\nPlease check your output.")
      } else if ((min(x.new) < min(x)) | (max(x.new) > max(x))){
        print("Note: new data exceed the domain of original data.")
      }
      tol.fit.order <- cbind(c(x , x.new),tol.out$fit)[order(c(x , x.new)),]
    }
  }
  
  if (dim(xy.data.original)[2] == 3){
    if (is.vector(x.new)) {
      stop("Note: 'x.new' needs to be in a matrix format with 2 columns.")
    } else if (is.null(x.new)){
      row.use <- dim(xy.data.original)[1]
    } else if (!is.null(x.new)) {
      row.use <- nrow(x.new)+(dim(xy.data.original)[1])
    }
  }
  
  alpha <- (tol.out$alpha.P)[1]
  P <- (tol.out$alpha.P)[2]
  ################################
  if (tol.out$reg.type == "nonparametric regression" | 
      tol.out$reg.type == "nonlinear regression"){
    ### Univariate X ###
    if (dim(xy.data.original)[2] == 2) {
      if (names(tol.out$fit)[3] == "1-sided.lower"){
        if (side == "upper"){
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Upper Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
        } else if (side == "lower") {
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Lower Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
          print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Lower Limit' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Upper Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
      } else {
        plot <- plot_ly() %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                    line = list(color = fit.col , size = fit.lwd) , 
                    name = 'Fitted Line' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = tol.col , size = tol.lwd) , 
                    name = 'Lower Limit' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = tol.col , size = tol.lwd) , 
                    name = 'Upper Limit' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("Two-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
    } else if (dim(xy.data.original)[2] == 3) {
      ### Bivariate X ###
      plot <- plot_ly()
      if (names(tol.out$fit)[3] == "1-sided.lower") {
        if (side == "upper"){
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,4], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Upper Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Upper Tolerance Plane", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        } else if (side == "lower"){
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,3], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Lower Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Lower Tolerance Plane", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        } else if (side == "two") {
          print("NOTE: The plot reflects two 1-sided tolerance planes and NOT 2-sided tolerance planes!")
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,3], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Lower Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,4], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Upper Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Tolerance Planes", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        }
      } else {
        plot <- plot %>%
          add_markers(plot,
                      x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=xy.data.original[,3], 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = 6) , 
                      name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,2], 
                    type = 'mesh3d' ,
                    facecolor = rep(fit.col , row.use^2), 
                    opacity = fit.opacity,
                    name = 'Fitted Plane' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,3], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , row.use^2), 
                    opacity = tol.opacity,
                    name = 'Lower Plane' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,4], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , row.use^2), 
                    opacity = tol.opacity,
                    name = 'Upper Plane' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("Two-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                      "% Tolerance Planes", sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)
            ),
            scene = list(xaxis = list(title = x.lab,
                                      tickfont = list(size = x.tick.size),
                                      titlefont = list(size = x.lab.size)),
                         yaxis = list(title = y.lab,
                                      tickfont = list(size = y.tick.size),
                                      titlefont = list(size = y.lab.size)),
                         zaxis = list(title = z.lab,
                                      tickfont = list(size = z.tick.size),
                                      titlefont = list(size = z.lab.size)))
          )
      }
      print(plot)
    }
  } else if (tol.out$reg.type == "linear regression"){
    ### Univariate X ###
    if (dim(xy.data.original)[2] == 2) {
      if (names(tol.out$model$coefficients)[1] != "(Intercept)") {
        print("NOTE: A regression through the origin is fitted!")
      }
      if (names(tol.out$fit)[3] == "1-sided.lower"){
        if (side == "upper"){
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Upper Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
        } else if (side == "lower") {
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Lower Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
          print("NOTE: The plot reflects two 1-sided tolerance intervals and NOT a 2-sided tolerance interval!")
          plot <- plot_ly() %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                      line = list(color = fit.col , size = fit.lwd) , 
                      name = 'Fitted Line' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Lower Limit' , showlegend = FALSE) %>%
            add_trace(x=tol.fit.order[,1] , 
                      y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                      line = list(color = tol.col , size = tol.lwd) , 
                      name = 'Upper Limit' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
      } else {
        plot <- plot_ly() %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order$y.hat , type = 'scatter' , mode = 'lines' ,
                    line = list(color = fit.col , size = fit.lwd) , 
                    name = 'Fitted Line' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order[,4] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = tol.col , size = tol.lwd) , 
                    name = 'Lower Limit' , showlegend = FALSE) %>%
          add_trace(x=tol.fit.order[,1] , 
                    y=tol.fit.order[,5] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = tol.col , size = tol.lwd) , 
                    name = 'Upper Limit' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("Two-Sided ",(1-alpha) * 100, "% / ", P * 100, 
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
    } else if (dim(xy.data.original)[2] == 3){
      if (names(tol.out$model$coefficients)[1] != "(Intercept)") {
        print("NOTE: A regression through the origin is fitted!")
      }
      ### Bivariate X ###
      plot <- plot_ly()
      if (names(tol.out$fit)[3] == "1-sided.lower") {
        if (side == "upper"){
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,4], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Upper Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Upper Tolerance Plane", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        } else if (side == "lower"){
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,3], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Lower Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Lower Tolerance Plane", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        } else if (side == "two") {
          print("NOTE: The plot reflects two 1-sided tolerance planes and NOT 2-sided tolerance planes!")
          plot <- plot %>%
            add_markers(plot,
                        x=xy.data.original[,1] , 
                        y=xy.data.original[,2] , 
                        z=xy.data.original[,3], 
                        type = 'scatter' , mode = 'markers' ,
                        marker = list(color = x.col , size = 6) , 
                        name = 'Data' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,2], 
                      type = 'mesh3d' ,
                      facecolor = rep(fit.col , row.use^2), 
                      opacity = fit.opacity,
                      name = 'Fitted Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,3], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Lower Plane' , showlegend = FALSE) %>%
            add_trace(x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=(tol.out$fit)[1:row.use,4], 
                      type = 'mesh3d' ,
                      facecolor = rep(tol.col , row.use^2), 
                      opacity = tol.opacity,
                      name = 'Upper Plane' , showlegend = FALSE) %>%
            layout(
              title = list(text = paste("One-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                        "% Tolerance Planes", sep = ""),
                           x = title.position.x,
                           y = title.position.y,
                           font = list(size=title.size)
              ),
              scene = list(xaxis = list(title = x.lab,
                                        tickfont = list(size = x.tick.size),
                                        titlefont = list(size = x.lab.size)),
                           yaxis = list(title = y.lab,
                                        tickfont = list(size = y.tick.size),
                                        titlefont = list(size = y.lab.size)),
                           zaxis = list(title = z.lab,
                                        tickfont = list(size = z.tick.size),
                                        titlefont = list(size = z.lab.size)))
            )
        }
      } else {
        plot <- plot %>%
          add_markers(plot,
                      x=xy.data.original[,1] , 
                      y=xy.data.original[,2] , 
                      z=xy.data.original[,3], 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = 6) , 
                      name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,2], 
                    type = 'mesh3d' ,
                    facecolor = rep(fit.col , row.use^2), 
                    opacity = fit.opacity,
                    name = 'Fitted Plane' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,3], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , row.use^2), 
                    opacity = tol.opacity,
                    name = 'Lower Plane' , showlegend = FALSE) %>%
          add_trace(x=xy.data.original[,1] , 
                    y=xy.data.original[,2] , 
                    z=(tol.out$fit)[1:row.use,4], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , row.use^2), 
                    opacity = tol.opacity,
                    name = 'Upper Plane' , showlegend = FALSE) %>%
          layout(
            title = list(text = paste("Two-Sided ",(1-alpha) * 100 , "% / " , P * 100,
                                      "% Tolerance Planes", sep = ""),
                         x = title.position.x,
                         y = title.position.y,
                         font = list(size=title.size)
            ),
            scene = list(xaxis = list(title = x.lab,
                                      tickfont = list(size = x.tick.size),
                                      titlefont = list(size = x.lab.size)),
                         yaxis = list(title = y.lab,
                                      tickfont = list(size = y.tick.size),
                                      titlefont = list(size = y.lab.size)),
                         zaxis = list(title = z.lab,
                                      tickfont = list(size = z.tick.size),
                                      titlefont = list(size = z.lab.size)))
          )
      }
      print(plot)
    }
  }
}
