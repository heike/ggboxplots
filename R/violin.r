##' Calculations for violin plots.
##'
##' Extract the values for plotting kernel density according to package vioplot
##' copied (except for return statement) verbatim from function vioplot, package vioplot v0.2
##'  
##' @param x continuous variable
##' @param at
##' @param h bandwidth for kernel density 
##' @param range
##' @param wex 
##' @return list of numeric descriptions of body and outliers
calc_violin <- function(x, at, h = NA, range = 1.5, wex = 1){
  datas <- list(x)
  n <- length(datas)
  if (missing(at)) {
    at <- 1:n
  }
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.na(h))){ 
    args <- c(args, h = h)
  }
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  i <- 1
  return(data.frame(x = c(base[[i]], rev(base[[i]])), y = c(at[i] - height[[i]], 
                                                            rev(at[i] + height[[i]]))))
}

##' Violin plots.
##' 
##' Create a violin plot within the ggplot2 framework
##'
##' Violin plots have been introduced by Hintze and Nelson (1998).
##' @references Hintze, Jerry L., and Ray D. Nelson. 1998. "Violin Plots: A Box Plot-Density Trace Synergism." The American Statistician 52(2):181-84
##' @param data dataset
##' @param x factor variable
##' @param y values
##' @param bandwidth value for kernel density
##' @param alpha parameter for alpha blending
##' @param fill color or symbol to fill in violins. 
##' @param ... other parameters 
##' @return ggplot2 layer of violins
##' @author Heike Hofmann
##' @export  
##' @examples
##' data(diamonds)
##' ggviolin(diamonds, color, price, bandwidth=500)
##' ggviolin(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
ggviolin <- function(data, x, y, bandwidth, alpha = 0.5, fill = "grey60", ...) {
  suppressMessages(require(plyr))
  arguments <- as.list(match.call()[-1])
  group <- eval(arguments$x, data)
  y <- eval(arguments$y, data)
  frame <- data.frame(group=group, y=y, data)
  if (!is.null(arguments$fill)) fill <- arguments$fill
  
  violins <- ddply(frame, .(group), function(x) {
    vio <- calc_violin(x$y, h = bandwidth)
    data.frame( x = vio$y + as.numeric(x$group[1])-1, y = vio$x,
                group = x$group[1])
  })
  p <- ggplot(violins, aes(x = x, y = y)) + xlab(arguments$x) + ylab(arguments$y)

  if (is.symbol(arguments$fill)) {
    p <- p + geom_polygon(aes(group = group, fill=group), alpha=1.25*alpha)
  } 
  else   p <- p + geom_polygon(aes(group = group), fill=fill, alpha=1.25*alpha)
  
  p <- p+  geom_boxplot(aes(x = as.numeric(group), y=y, group = group), colour="grey60", width = .1, data=frame) +
    scale_x_continuous(breaks= 1:length(levels(frame$group)), labels=levels(frame$group))
  
  p
}