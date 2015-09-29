
#' @importFrom grid grobTree
GeomVase <- ggplot2::ggproto("GeomVase", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(weight = 1, colour = "grey30", fill = "grey70", size = 0.5, 
                             alpha = NA, width = 0.75, shape = 16, linetype = "solid", outlier.colour = "black",
                             outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5),
  draw_key = ggplot2::draw_key_rect,
  
  draw_group = function(data, panel_scales, coord, ...) {
    fivenum <- data$fivenum[[1]]
    
    common <- unique(data.frame(
      colour = data$colour, 
      size = data$size, 
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),  
      group = NA, 
      stringsAsFactors = FALSE
    ))
    
    whiskers <- data.frame(
      x = fivenum$x,
      xend = fivenum$x, 
      y = c(fivenum$upper, fivenum$lower), 
      yend = c(fivenum$ymax, fivenum$ymin),
      alpha = NA,
      common, row.names=NULL)
    
    if (!is.null(fivenum$outliers) && length(fivenum$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = fivenum$outliers[[1]],
        x = data$x[1],
        colour = data$outlier.colour[1] %||% data$colour[1],
        shape = data$outlier.shape[1] %||% data$shape[1],
        size = data$outlier.size[1] %||% data$size[1],
        stroke = data$outlier.stroke[1] %||% data$stroke[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE)
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_scales, coord)
    } else {
      outliers_grob <- NULL
    }
    
    # reduce data to box area:
    data <- data[(data$y >= fivenum$lower) & (data$y <= fivenum$upper),]
    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x - vasewidth * (x-xmin),
                      xmaxv = x + vasewidth * (xmax-x))
    
    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(arrange(transform(data, x = xminv), y),
                     arrange(transform(data, x = xmaxv), -y))
    
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    
    ydiff <- diff(data$y)[1]
    middle <- data[data$y >= (fivenum$middle - ydiff) & (data$y <= fivenum$middle+ydiff),]
    
    medians <- data.frame(
      x = mean(middle$xminv),
      xend = mean(middle$xmaxv), 
      y = fivenum$middle, 
      yend = fivenum$middle,
      alpha = NA,
      common, row.names=NULL)
 #   medians$size <- medians$size * fatten
    
        
    ggplot2:::ggname("geom_vase", grobTree(
      outliers_grob,
      GeomPolygon$draw_panel(newdata, panel_scales, coord),
#      outliers_grob,
      GeomSegment$draw_panel(whiskers, panel_scales, coord),
      GeomSegment$draw_panel(medians, panel_scales, coord)
    ))
  },
  
  draw_legend = function(data, ...)  {
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }
)

#' Side by side vase plots.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_boxplot
#' @inheritParams ggplot2::geom_violin
#' @export
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' 
#' p + geom_vase()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "vase")
#' 
#' p + geom_vase() + geom_jitter(height = 0)
#' p + geom_vase() + coord_flip()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "vase") +
#'   coord_flip()
#' 
#' # Scale maximum width proportional to sample size:
#' p + geom_vase(scale = "count")
#'
#' # Scale maximum width to 1 for all violins:
#' p + geom_vase(scale = "width")
#' 
#' # Use a smaller bandwidth for closer density fit (default is 1).
#' p + geom_vase(adjust = .5)
#' 
#' # Add aesthetic mappings
#' # Note that vases are automatically dodged when any aesthetic is 
#' # a factor
#' p + geom_vase(aes(fill = cyl)) 
#' p + geom_vase(aes(fill = factor(cyl)))
#' p + geom_vase(aes(fill = factor(vs))) # doesn't work
#' p + geom_vase(aes(fill = factor(am))) # doesn't work
#' 
#' # Set aesthetics to fixed value
#' p + geom_vase(fill = "grey80", colour = "#3366FF")
#' qplot(factor(cyl), mpg, data = mtcars, geom = "vase", 
#'   colour = I("#3366FF"))
#' 
#' # Scales vs. coordinate transforms -------
#' # Scale transformations occur before the density statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' library(plyr) # to access round_any
#' m <- ggplot(movies, aes(y = votes, x = rating,
#'    group = round_any(rating, 0.5)))
#' m + geom_vase()
#' m + geom_vase() + scale_y_log10()
#' m + geom_vase() + coord_trans(y = "log10")
#' m + geom_vase() + scale_y_log10() + coord_trans(y = "log10")
#' 
#' # Vase plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' qplot(year, budget, data = movies, geom = "vase")
#' qplot(year, budget, data = movies, geom = "vase", 
#'   group = round_any(year, 10, floor))
#' 
#' x <- rnorm(2000)
#' group <- rep(1:20, 100)
#' y <- rep(c(1,2),1000)
#' qplot(y,x,facets=~group, fill=factor(y), geom="vase") 
#' }
geom_vase <- function (mapping = NULL, data = NULL, stat = "vase", position = "dodge", show.legend = NA, inherit.aes = TRUE, width = 0.9, alpha = 0.25,
                       ...) {
  layer(
    geom = GeomVase, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, ...)
  )
}
