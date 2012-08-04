##' Calculations for vase plots.
##'
##' @param x continuous variable
##' @param ... other variables
##' @param names labels
##' @param bw bandwidth for kernel density 
##' @return list of numeric descriptions of body, whiskers, outliers and the median
##' @author Kayla
calc_vase <- function(x, ..., names = NULL, bw = NULL) {
  all.x <- c(x, list(...))

  centers <- seq_along(all.x)
  n <- length(all.x)
  if (is.null(names)) {
    names <- names(all.x)
  }

  vase <- list()
  for(i in 1:n) {
    lower <- quantile(all.x[[i]], probs = .25)
    upper <- quantile(all.x[[i]], probs = .75)
    Hspread <- (upper - lower)[[1]]
    step <- 1.5*Hspread[[1]]
    median <- median(all.x[[i]])

    ###tukey definition of whiskers
    TUex <- max(all.x[[i]][all.x[[i]] <= upper+step])
    TLex <- min(all.x[[i]][all.x[[i]] >= lower-step])
    
    ds <- density(all.x[[i]], bw = bw[i])
    Xs <- ds$x
    Ys <- ds$y
    
    in_box <- Xs < upper & Xs > lower
    Xs <- c(lower, Xs[in_box], upper)
    Ys <- c(0, Ys[in_box], 0)
    
    # Scale to 0.4
    Ys <- Ys / max(Ys) * 0.4
    
    mpos <- which.min(abs(Xs - median))[1]
    outliers <- (all.x[[i]] > upper+step) |	
      (all.x[[i]] < lower-step)
    
    
    attr(Xs,"names") <- NULL
    vase[[i]] <- list(body=cbind(x=c(centers[i]+Ys, rev(centers[i]-Ys)), 
                                 y=c(Xs, rev(Xs))),
                      median=cbind(x=(centers[i]-Ys)[mpos], 
                                   y=Xs[mpos], 
                                   xend=(centers[i]+Ys)[mpos], 
                                   yend=Xs[mpos]),
                      whisker=cbind(x=rep(centers[i], 2),
                                    xend=rep(centers[i], 2), 
                                    y=c(lower, upper),
                                    yend=c(TLex, TUex)), 
                      outliers=cbind(x=rep(centers[i], sum(outliers)), 
                                     y=all.x[[i]][outliers])
    )   
  }

  
  invisible(vase)
}

##' Vase plots.
##' 
##' Create a vase plot within the ggplot2 framework
##'
##' Need the reference to the Benjamini paper.
##' @param data dataset
##' @param x factor variable
##' @param y continuous variable
##' @param ... other parameters 
##' @return ggplot2 layer of vases
##' @author Heike Hofmann
##' @export  
##' @examples
##' data(diamonds)
##' ggvase(diamonds, color, price, bandwidth=500)
##' ggvase(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
ggvase <- function(data, x, y, bandwidth, ...) {
  suppressMessages(require(plyr))
  arguments <- as.list(match.call()[-1])
  group <- eval(arguments$x, data)
  y <- eval(arguments$y, data)
  frame <- data.frame(group=group, y=y)
  
  fill <- "grey50"
  fillaes <- NULL
  
  if (!is.null(arguments$fill)) {
    if (is.symbol(arguments$fill)) { 
      fillaes <- arguments$fill
      frame$fill <- eval(arguments$fill, data)                                   
    } else fill <- arguments$fill
  }  
  
  vd <- dlply(frame, .(group), function(x) {
    vd <- calc_vase(x=list(x$y), bw = bandwidth)
    df <- data.frame(vd[[1]]$body)
    df$group <- x$group[1]
    df$x <- df$x + as.numeric(df$group)[1]
    df$order <- 1:nrow(df)
#    browser()
    if (!is.null(fillaes)) df$fill <- x$fill[1]
    body <- df
    
    df <- data.frame(vd[[1]]$median)
    df$group <- x$group[1]
    df$x <- df$x + as.numeric(df$group)
    df$xend <- df$xend + as.numeric(df$group)
    median <- df
    
    df <- data.frame(vd[[1]]$whisker)
    df$group <- x$group[1]
    df$x <- df$x + as.numeric(df$group)
    df$xend <- df$xend + as.numeric(df$group)
    whisker <- df 
    
    df <- data.frame(vd[[1]]$outlier)
    if (nrow(df) > 0) {
      df$group <- x$group[1]
      df$x <- df$x + as.numeric(df$group)
    }
    outlier <- df
    
    
    return(list(body=body, median=median, whisker=whisker, outlier=outlier))
  })
  
  vdbody <- ldply(1:length(vd), function(x) {
    df <- data.frame(vd[[x]]$body)
    df$order <- 1:nrow(df)
    df
  })
  
  vdoutlier <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$outlier)
  )
  
  vdmedian <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$median)
  )
  
  vdwhisker <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$whisker)
  )
#  fill=factor(group), colour=factor(group)),
  if (!is.null(fillaes)) p <- geom_polygon(aes(x, y, group=group, fill=group),  data=vdbody,  colour="grey80", alpha=0.5)
  else p <- geom_polygon(aes(x, y, group=group), fill=fill,  data=vdbody,  colour="grey80", alpha=0.5)
  p <- list(p,
    geom_segment(aes(x, y, group=group, xend=xend,yend=yend), data=vdmedian),
    geom_segment(aes(x, y, group=group, xend=xend,yend=yend), data=vdwhisker), 
    scale_x_discrete(arguments$x, breaks=1+1:length(levels(group)), labels=levels(group), expand=c(0,0)),
    scale_y_continuous(arguments$y))
    
  if (!is.null(vdoutlier))	
    p <- list(p, geom_point(aes(x, y), colour="grey10", size=1.5, data=vdoutlier))
  
  ggplot() + p
}