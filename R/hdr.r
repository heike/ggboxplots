##' High Density Region plots.
##' 
##' Create a high density region boxplots (Hyndman,  1996) within the ggplot2 framework. HDR plots are based on a density estimate of the marginal distribution. 
##' Cutoff values for the probability are defined in the parameter \code{probs} to define regions. In a uni-modal situation, the HDR plots with probability \code{probs=0.25} 
##' show the boxes of a regular boxplot. The code is based on the \code{hdrcde} package
##'
##' @param data dataset
##' @param x factor variable
##' @param y values
##' @param probs vector of probabilities as cut-offs for the density regions
##' @param fill fill color of highest density regions. Either a color description (name or hex string), or grouping variable.
##' @param ... other parameters passed on to the ggplot2 framework
##' @return ggplot2 layer of highest density region boxplots.
##' @author Heike Hofmann
##' @cite hyndman
##' @export  
##' @examples
##' data(diamonds)
##' gghdr(diamonds, color, price)
##' gghdr(diamonds, cut, price, probs=c(50,25,12.5, 6.25), fill=cut) + 
##'   scale_fill_brewer(palette="Set1") + 
##'   scale_colour_brewer(palette="Set1")
gghdr <- function(data, x, y, probs= c(90, 50, 25), fill="grey50", ...) {
  arguments <- as.list(match.call()[-1])
  group <- eval(arguments$x, data)
  y <- eval(arguments$y, data)
  frame <- data.frame(group=group, y=y, data)
  if (!is.null(arguments$fill)) frame$fill <- eval(arguments$fill, data)
#  facets <- NULL
#  if (!is.null(arguments$facets)) facets <- "cut"
  x1=x2=NULL
  
  hdr.df <- ddply(frame, .(group), function(x) {
    res <- hdr(x$y, prob=probs)
    m <- res$hdr
    k <- dim(m)[2]/2
    out <- data.frame(x1=m[,1], x2=m[,2])
    if (k > 1)
      for (i in 2:k) 
        out <- rbind(out, data.frame(x1=m[,2*i-1], x2=m[,2*i]))
    out$probs <- probs
    out$group <- x$group[1]
    if (is.symbol(arguments$fill))
      out$fill <- x$fill[1]
    out$mode <- res$mode
    out  
  })
  hdr.df <- na.omit(hdr.df)
  
  outliers <- ddply(frame, .(group), function(x) {
    outsub <- subset(hdr.df, group==x$group[1])
    res <- x[x$y > max(outsub$x2) | x$y < min(outsub$x1),]
    res
  })

  if (is.symbol(arguments$fill)) {
    p <- ggplot(aes(fill=group), data=hdr.df) + 
      geom_rect(aes(xmin=as.numeric(group)-0.4, 
       xmax=as.numeric(group)+0.4, 
       ymin=x1, ymax=x2, fill=group), alpha=0.5) + 
         geom_segment(aes(x=as.numeric(group)-0.45,
                          xend=as.numeric(group)+0.45,
                          y=mode, yend=mode,
                          colour=group))   
  } 
  else   
    p <- ggplot(data=hdr.df) + 
      geom_rect(aes(xmin=as.numeric(group)-0.4, 
      xmax=as.numeric(group)+0.4, 
      ymin=x1, ymax=x2), fill=fill, alpha=0.5) + 
        geom_segment(aes(x=as.numeric(group)-0.45,
                         xend=as.numeric(group)+0.45,
                         y=mode, yend=mode
        ))  
  
  p <- p + geom_point(aes(as.numeric(group), y), data=outliers) +
    scale_x_continuous(arguments$x, breaks=1:length(levels(group)), labels=levels(group)) +
    ylab(arguments$y)
  p
}