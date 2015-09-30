"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' High Density Region plots.
#' 
#' High density region (HDR) boxplots are a variation of boxplots.  HDR plots are based on a density estimate of the marginal distribution. 
#' Cutoff values for the probability are defined in the parameter \code{probs} to define regions. In a uni-modal situation, the HDR plots with probability \code{probs=0.25} 
#' show the boxes of a regular boxplot. The code is based on the \code{hdrcde} package by \cite{hyndman}, who also introduced the plots.
#' @inheritParams ggplot2::stat_density
#' @inheritParams ggplot2::stat_boxplot
#' @inheritParams ggplot2::stat_identity
#' @param scale if "area" (default), all vases have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all vases have the same maximum width.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#'
#' @return A data frame with additional columns:
#'   \item{density}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points - not sure about usefulness for vase plots}
#'   \item{vasewidth}{density scaled for the vase plot, according to area, counts
#'                      or to a constant maximum width}
#'   \item{n}{number of points}
#'   \item{width}{width of vase bounding box}
#' @seealso \code{\link{geom_hdr}} for examples, and \code{\link{geom_violin}}
#'   for examples with data along the x axis.
#' @export
#' @examples
#' # See geom_hdr for examples
#' # Also see stat_density for similar examples with data along x axis
stat_hdr <- function(mapping = NULL, data = NULL, geom = "hdr",
                     position = "dodge", show.legend = NA, inherit.aes = TRUE, 
                     width = 0.9, probs=c(0.99, 0.95, 0.5), ...) {
  layer(
    stat = StatHdr, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      probs = probs,
      width = width, 
      na.rm=na.rm, 
      ...
    )
  )
}


#' @export 
StatHdr <- ggplot2::ggproto("StatHdr", ggplot2::Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",
  
  setup_data = function(data, params) {
    data <- remove_missing(data, na.rm, "y", name = "stat_hdr", finite = TRUE)
    
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    plyr::ddply(data, "group", transform,
                ymin = min(y),
                ymax = max(y),
                xmin = x - width / 2,
                xmax = x + width / 2
    )
  },                    
  compute_group = function(data, scales, params, na.rm = FALSE, width = 0.9, 
                           probs=c(0.99, 0.95, 0.5), ...) {

    res <- hdr(data$y, prob=probs*100)
    
    common <- unique(data[,c("x", "PANEL", "group", "xmin", "xmax")])
    
    m <- res$hdr
    k <- dim(m)[2]/2
    out <- data.frame(ymin=m[,1], ymax=m[,2])
    if (k > 1)
      for (i in 2:k) 
        out <- rbind(out, data.frame(ymin=m[,2*i-1], ymax=m[,2*i]))
    out$probs <- probs

    out$mode <- res$mode
    row.names(out) <- 1:nrow(out)
    out <- data.frame(common, out, row.names=NULL)
    
    browser()
    idx <- which(data$y > max(out$ymax) | data$y < min(out$ymin))
    if (length(idx) > 0) {
      outliers <- data$y[idx]
      out$out <- I(list(outliers))
    } else out$out <- I(list(0))

    out$ymin_final <- min(data$y, na.rm=T)
    out$ymax_final <- max(data$y, na.rm=T)
    
    out 
  }
)
