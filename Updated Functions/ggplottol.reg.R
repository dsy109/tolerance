ggplottol.reg <- function (tol.out,
                           x,
                           new.x = NULL,
                           y,
                           side = c("two","upper", "lower"),
                           rect = FALSE,
                           smooth = 4,
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
  ### Check data dimensions ###
  xy.data.original <- as.data.frame(cbind(y,x))
  
  if (dim(xy.data.original)[2] > 3){
    stop("Plotting is only available for 1 or 2 predictors.")
  } else if (dim(xy.data.original)[2] == 2){
    if (is.null(new.x)){
      tol.order <- cbind(x,tol.out$tol[match(y,tol.out$tol$y),])[order(x),]
    } else if (!is.null(new.x)) {
      new.x <- as.data.frame(new.x)
      new.to.use <- match(new.x[,1] , tol.out$newdata[,1])
      if (sum(is.na(new.to.use)) > 0) {
        stop("No toletance limits generated for new data.")
      } else if ((min(new.x) < min(x)) | (max(new.x) > max(x))){
        print("NOTE: new data exceed the domain of original data.")
      }
      tol.order <- cbind(c(x , new.x[,1]) , 
                         tol.out$tol[c(1:(dim(xy.data.original)[1]) , 
                                       (new.to.use+dim(xy.data.original)[1])),])[order(c(x , new.x[,1])),]
    }
  } else if (dim(xy.data.original)[2] == 3){
    if (is.null(new.x)){
      if (!rect){
        x.all <- xy.data.original[,2:3]
        tol.all <- tol.out$tol[c(1:dim(xy.data.original)[1]),]
      } else if (rect){
        if (tol.out$reg.type == "npreg"){
          smooth <- 3
        }
        x1.seq <- seq(from = min(xy.data.original[,2]),
                      to = max(xy.data.original[,2]) , length = smooth)
        x2.seq <- seq(from = min(xy.data.original[,3]),
                      to = max(xy.data.original[,3]) , length = smooth)
        rect.matrix <- as.matrix(rbind(cbind(x1.seq[1] , x2.seq),
                                       cbind(x1.seq[smooth] , x2.seq),
                                       cbind(x1.seq[-c(1,smooth)] , x2.seq[1]),
                                       cbind(x1.seq[-c(1,smooth)] , x2.seq[smooth])))
        if (tol.out$reg.type == "linreg"){
          corner.tol <- regtol.int2(reg = tol.out$model , new.x = rect.matrix ,
                                    alpha = tol.out$alpha.P.side[1],
                                    P = tol.out$alpha.P.side[2],
                                    side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) , rect.matrix)
          tol.all <- rbind(tol.out$tol[1:dim(xy.data.original)[1],] , 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        } else if (tol.out$reg.type == "nlreg"){
          corner.tol <- nlregtol.int2(formula = as.formula(tol.out$model) ,
                                      xy.data = tol.out$xy.data.original,
                                      new.x = rect.matrix ,
                                      alpha = tol.out$alpha.P.side[1],
                                      P = tol.out$alpha.P.side[2],
                                      side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) , rect.matrix)
          tol.all <- rbind(tol.out$tol[1:dim(xy.data.original)[1],] , 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        } else if (tol.out$reg.type == "npreg"){
          cat("NOTE: Tolerance limits for corner points are estimated based on linear model.
              \n'rect=FALSE' is recommanded. If 'rect=TRUE', 'smooth' paramter is fixed at value of 3.\n")
          lower.npreg <- tol.out$lower.upper[1]
          upper.npreg <- tol.out$lower.upper[2]
          if (is.na(lower.npreg)){
            lower.npreg <- NULL
          } else {lower.npreg <- as.numeric(lower.npreg)}
          if (is.na(upper.npreg)){
            upper.npreg <- NULL
          } else {upper.npreg <- as.numeric(upper.npreg)}
          y.npreg <- xy.data.original[,1]
          x1.npreg <- xy.data.original[,2]
          x2.npreg <- xy.data.original[,3]
          corner.tol <- regtol.int2(reg = lm(y.npreg ~ x1.npreg + x2.npreg) , 
                                    new.x = as.data.frame(rect.matrix) ,
                                    alpha = tol.out$alpha.P.side[1],
                                    P = tol.out$alpha.P.side[2],
                                    side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) , rect.matrix)
          tol.all <- rbind(tol.out$tol[1:dim(xy.data.original)[1],] , 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        }
      }
    } else if (!is.null(new.x)){
      new.x <- as.data.frame(new.x)
      names(new.x) <- names(xy.data.original)[2:3]
      
      if (dim(new.x)[2] != 2){
        stop("Please check the dimension of new data.")
      }
      
      if (!rect){
        x.all <- rbind(xy.data.original[,2:3] , new.x)
        tol.all <- tol.out$tol
      } else if (rect){
        if (tol.out$reg.type == "npreg"){
          smooth <- 3
        }
        x1.seq <- seq(from = min(xy.data.original[,2] , new.x[,1]),
                      to = max(xy.data.original[,2] , new.x[,1]) , length = smooth)
        x2.seq <- seq(from = min(xy.data.original[,3] , new.x[,2]),
                      to = max(xy.data.original[,3] , new.x[,2]) , length = smooth)
        rect.matrix <- as.matrix(rbind(cbind(x1.seq[1] , x2.seq),
                                       cbind(x1.seq[smooth] , x2.seq),
                                       cbind(x1.seq[-c(1,smooth)] , x2.seq[1]),
                                       cbind(x1.seq[-c(1,smooth)] , x2.seq[smooth])))
        if (tol.out$reg.type == "linreg"){
          corner.tol <- regtol.int2(reg = tol.out$model , new.x = rect.matrix ,
                                    alpha = tol.out$alpha.P.side[1],
                                    P = tol.out$alpha.P.side[2],
                                    side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) ,
                         as.matrix(new.x) , 
                         rect.matrix)
          tol.all <- rbind(tol.out$tol, 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        } else if (tol.out$reg.type == "nlreg"){
          corner.tol <- nlregtol.int2(formula = as.formula(tol.out$model) ,
                                      xy.data = tol.out$xy.data.original,
                                      new.x = rect.matrix ,
                                      alpha = tol.out$alpha.P.side[1],
                                      P = tol.out$alpha.P.side[2],
                                      side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) ,
                         as.matrix(new.x) , 
                         rect.matrix)
          tol.all <- rbind(tol.out$tol, 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        } else if (tol.out$reg.type == "npreg"){
          cat("NOTE: New data cannot be predicted for nonparametric tolerance.
              \nCorner points are estimated based linear model.\n 
              \n'rect=FALSE' is recommanded. If 'rect=TRUE', 'smooth' paramter is fixed at value of 3.\n")
          lower.npreg <- tol.out$lower.upper[1]
          upper.npreg <- tol.out$lower.upper[2]
          if (is.na(lower.npreg)){
            lower.npreg <- NULL
          } else {lower.npreg <- as.numeric(lower.npreg)}
          if (is.na(upper.npreg)){
            upper.npreg <- NULL
          } else {upper.npreg <- as.numeric(upper.npreg)}
          y.npreg <- xy.data.original[,1]
          x1.npreg <- xy.data.original[,2]
          x2.npreg <- xy.data.original[,3]
          corner.tol <- regtol.int2(reg = lm(y.npreg ~ x1.npreg + x2.npreg) , 
                                    new.x = as.data.frame(rect.matrix) ,
                                    alpha = tol.out$alpha.P.side[1],
                                    P = tol.out$alpha.P.side[2],
                                    side = tol.out$alpha.P.side[3] , new=TRUE)
          x.all <- rbind(as.matrix(xy.data.original[,2:3]) , rect.matrix)
          tol.all <- rbind(tol.out$tol[1:dim(xy.data.original)[1],] , 
                           corner.tol$tol[-c(1:dim(xy.data.original)[1]),])
        }
      } 
    }
    }
  
  alpha <- (tol.out$alpha.P)[1]
  P <- (tol.out$alpha.P)[2]
  #############################################################
  ### Univariate Data ###
  if (dim(xy.data.original)[2] == 2){
    if (tol.out$reg.type == "linreg"){
      if (names(tol.out$model$coefficients)[1] != "(Intercept)") {
        print("NOTE: A regression through the origin is fitted!")
      }
    }
    if (names(tol.out$tol)[3] == "1-sided.lower"){
      if (side == "upper"){
        plot <- plot_ly() %>%
          add_trace(x=xy.data.original[,2] , 
                    y=xy.data.original[,1] , 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order$y.hat , type = 'scatter' , mode = 'lines' ,
                    line = list(color = fit.col , size = fit.lwd) , 
                    name = 'Fitted Line' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order[,5] , type = 'scatter' , mode = 'lines' ,
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
          add_trace(x=xy.data.original[,2] , 
                    y=xy.data.original[,1] , 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order$y.hat , type = 'scatter' , mode = 'lines' ,
                    line = list(color = fit.col , size = fit.lwd) , 
                    name = 'Fitted Line' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order[,4] , type = 'scatter' , mode = 'lines' ,
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
          add_trace(x=xy.data.original[,2] , 
                    y=xy.data.original[,1] , 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order$y.hat , type = 'scatter' , mode = 'lines' ,
                    line = list(color = fit.col , size = fit.lwd) , 
                    name = 'Fitted Line' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order[,4] , type = 'scatter' , mode = 'lines' ,
                    line = list(color = tol.col , size = tol.lwd) , 
                    name = 'Lower Limit' , showlegend = FALSE) %>%
          add_trace(x=tol.order[,1] , 
                    y=tol.order[,5] , type = 'scatter' , mode = 'lines' ,
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
        add_trace(x=xy.data.original[,2] , 
                  y=xy.data.original[,1] , 
                  type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=tol.order[,1] , 
                  y=tol.order$y.hat , type = 'scatter' , mode = 'lines' ,
                  line = list(color = fit.col , size = fit.lwd) , 
                  name = 'Fitted Line' , showlegend = FALSE) %>%
        add_trace(x=tol.order[,1] , 
                  y=tol.order[,4] , type = 'scatter' , mode = 'lines' ,
                  line = list(color = tol.col , size = tol.lwd) , 
                  name = 'Lower Limit' , showlegend = FALSE) %>%
        add_trace(x=tol.order[,1] , 
                  y=tol.order[,5] , type = 'scatter' , mode = 'lines' ,
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
    ### Bivariate Data ###
    if (tol.out$reg.type == "linreg"){
      if (names(tol.out$model$coefficients)[1] != "(Intercept)") {
        print("NOTE: A regression through the origin is fitted!")
      }
    }
    plot <- plot_ly()
    if (names(tol.out$tol)[3] == "1-sided.lower") {
      if (side == "upper"){
        plot <- plot %>%
          add_markers(plot,
                      x=xy.data.original[,2] , 
                      y=xy.data.original[,3] , 
                      z=xy.data.original[,1], 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,2], 
                    type = 'mesh3d' ,
                    facecolor = rep(fit.col , dim(x.all)[1]^2), 
                    opacity = fit.opacity,
                    name = 'Fitted Plane' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,4], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , dim(x.all)[1]^2), 
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
                      x=xy.data.original[,2] , 
                      y=xy.data.original[,3] , 
                      z=xy.data.original[,1], 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,2], 
                    type = 'mesh3d' ,
                    facecolor = rep(fit.col , dim(x.all)[1]^2), 
                    opacity = fit.opacity,
                    name = 'Fitted Plane' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,3], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , dim(x.all)[1]^2), 
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
                      x=xy.data.original[,2] , 
                      y=xy.data.original[,3] , 
                      z=xy.data.original[,1], 
                      type = 'scatter' , mode = 'markers' ,
                      marker = list(color = x.col , size = x.cex) , 
                      name = 'Data' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,2], 
                    type = 'mesh3d' ,
                    facecolor = rep(fit.col , dim(x.all)[1]^2), 
                    opacity = fit.opacity,
                    name = 'Fitted Plane' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,4], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , dim(x.all)[1]^2), 
                    opacity = tol.opacity,
                    name = 'Upper Plane' , showlegend = FALSE) %>%
          add_trace(x=x.all[,1] , 
                    y=x.all[,2] , 
                    z=tol.all[,3], 
                    type = 'mesh3d' ,
                    facecolor = rep(tol.col , dim(x.all)[1]^2), 
                    opacity = tol.opacity,
                    name = 'Lower Plane' , showlegend = FALSE) %>%
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
                    x=xy.data.original[,2] , 
                    y=xy.data.original[,3] , 
                    z=xy.data.original[,1], 
                    type = 'scatter' , mode = 'markers' ,
                    marker = list(color = x.col , size = x.cex) , 
                    name = 'Data' , showlegend = FALSE) %>%
        add_trace(x=x.all[,1] , 
                  y=x.all[,2] , 
                  z=tol.all[,2], 
                  type = 'mesh3d' ,
                  facecolor = rep(fit.col , dim(x.all)[1]^2), 
                  opacity = fit.opacity,
                  name = 'Fitted Plane' , showlegend = FALSE) %>%
        add_trace(x=x.all[,1] , 
                  y=x.all[,2] , 
                  z=tol.all[,4], 
                  type = 'mesh3d' ,
                  facecolor = rep(tol.col , dim(x.all)[1]^2), 
                  opacity = tol.opacity,
                  name = 'Upper Plane' , showlegend = FALSE) %>%
        add_trace(x=x.all[,1] , 
                  y=x.all[,2] , 
                  z=tol.all[,3], 
                  type = 'mesh3d' ,
                  facecolor = rep(tol.col , dim(x.all)[1]^2), 
                  opacity = tol.opacity,
                  name = 'Lower Plane' , showlegend = FALSE) %>%
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
    plot
  }
}
