"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' Combination of boxplot and 1d kernel density estimate along y axis, for vase plot.
#'
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
#' @seealso \code{\link{geom_vase}} for examples, and \code{\link{geom_violin}}
#'   for examples with data along the x axis.
#' @export
#' @examples
#' # See geom_vase for examples
#' # Also see stat_density for similar examples with data along x axis
stat_vase <- function(mapping = NULL, data = NULL, geom = "vase",
                      position = "dodge",adjust=1, kernel="gaussian", 
                      scale="area", show.legend = NA, inherit.aes = TRUE, width = 0.9, ...) {
  layer(
    stat = StatVase, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      adjust = adjust,
      kernel = kernel,
      scale = scale,
      width = width, 
      na.rm=na.rm, 
      ...
    )
  )
}

#' @export 
StatVase <- ggplot2::ggproto("StatVase", ggplot2::Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",
  
  setup_data = function(data, params) {
    data <- remove_missing(data, na.rm, "y", name = "stat_vase", finite = TRUE)
    
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
                           scale = "area", adjust = 1,
                           kernel = "gaussian", ...) {
    data$weight <- data$weight %||% 1
    
    width <- width %||%  resolution(data$x, FALSE) 
    
    fivenum <- StatBoxplot$compute_group(data=data, width=width, ...)
    data$weight <- 1/sum(data$weight)
    dens <- suppressWarnings(StatYdensity$compute_group(data=data, adjust=adjust, ...))
    dens$fivenum <- I(list(fivenum))
    dens
  },
  compute_panel = function(self, data, scales, params, na.rm = FALSE, width = 0.9, 
                           scale = "area", adjust = 1,
                           kernel = "gaussian", ...) {

    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, adjust = adjust, kernel = kernel,
      na.rm = na.rm, params = params, width=width, ...
    )
    
    scale <- match.arg(scale, c("area", "count", "width"))
    data$vasewidth <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = (data$density / max(data$density)) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )

    data
  }
  
  
)
