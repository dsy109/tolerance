plotly_multitol <- function (tol.out,
                             x,
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
                             tol.col = NULL,
                             tol.lwd = NULL,
                             tol.line.type = c("dash","dot","dashdot","solid"),
                             title = NULL,
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
  
  if (is.matrix(x)) {
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
  
  if (ncol(x) == 2) {
    mu <- apply(x, 2, mean)
    sigma <- cov(x)
    es <- eigen(sigma)
    e1 <- es$vec %*% diag(sqrt(es$val))
    theta <- seq(0, 2 * pi, len = 1000)
    r1 <- sqrt(tol.out[1, 1])
    v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
    pts <- t(mu - (e1 %*% t(v1)))
    
    if (is.null(title)){
      title <- paste("(P,",'&#947;',")=(",P,",",(alpha), 
                       ") Tolerance Region", sep = "")
    }
    plot_ly() %>%
      add_trace(x=x[,1] , y=x[,2] , type = 'scatter' , mode = 'markers' ,
                marker = list(color = x.col , size = x.cex) , 
                name = 'Data' , showlegend = FALSE) %>%
      add_trace(x=pts[,1] , y=pts[,2] , type = 'scatter' , mode = 'lines' ,
                line = list(color = tol.col , width = tol.lwd , dash=tol.line.type) , 
                name = 'Tolerance Region' , showlegend = FALSE) %>%
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
      )
  }
  else if (ncol(x) == 3) {
    Sigma <- cov(x)
    Mean <- apply(x, 2, mean)
    es <- eigen(Sigma)
    e1 <- es$vec %*% diag(sqrt(es$val))
    theta <- seq(0, pi, len = 100)
    phi <- seq(0, 2 * pi, len = 100)
    theta.phi <- expand.grid(theta,phi)
    r1 <- sqrt(tol.out[1, 1])
    v1 <- cbind(r1 * sin(theta.phi[,1])*cos(theta.phi[,2]), r1 * sin(theta.phi[,1])*sin(theta.phi[,2]), r1 * cos(theta.phi[,1]))
    pts <- t(Mean - (e1 %*% t(v1)))
    ellipse.x <- pts[,1]
    ellipse.y <- pts[,2]
    ellipse.z <- pts[,3]
    
    updatemenus <- list(
      list(
        active = 0,
        type= 'buttons',
        buttons = list(
          list(
            label = "Show Tolerance Ellipsoid",
            method = "update",
            args = list(list(visible = c(TRUE, TRUE)))),
          list(
            label = "Hide Tolerance Ellipsoid",
            method = "update",
            args = list(list(visible = c(TRUE, FALSE)))))
      )
    )
    
    if (is.null(title)){
      title <- paste("(P,",'&#947;',")=(",P,",",(alpha), 
                       ") Tolerance Region", sep = "")
    }
    
    plot_ly() %>%
      add_markers(x=x[,1] , y=x[,2] , z=x[,3], type = 'scatter' , mode = 'markers' ,
                  marker = list(color = x.col , size = x.cex) , 
                  name = 'Data' , showlegend = FALSE) %>%
      add_trace(type='mesh3d', alphahull = 0, opacity = 0.25, size = 6, 
                x = ellipse.x, y = ellipse.y, z = ellipse.z, facecolor = rep(tol.col , dim(x)[1]^3),
                name = 'Tolerance Region' , showlegend = FALSE) %>%
      layout(
        updatemenus = updatemenus,
        title = list(text = title,
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
}
