
#' @importFrom grid grobTree
GeomHdr <- ggplot2::ggproto("GeomHdr", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  default_aes = ggplot2::aes(weight = 1, colour = "grey40", fill = "grey60", size = 0.5, 
                             alpha = 0.5, width = 0.75, shape = 16, linetype = "solid", outlier.colour = "black",
                             outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5),
  draw_key = ggplot2::draw_key_rect,
  
  draw_group = function(data, panel_scales, coord, fatten = 2, ...) {

    common <- unique(data.frame(
      colour = data$colour, 
      size = data$size, 
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),  
      group = 1, 
      stringsAsFactors = FALSE
    ))
    
    outliers <- data$out[[1]]
    if (!is.null(outliers) && length(outliers) >= 1) {
      outliers <- data.frame(
        y = outliers,
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
    medians <- unique(data[,c("xmin", "xmax", "mode", "size")])
    medians$alpha <- NA
    medians <- data.frame(medians, common, row.names=NULL)
    medians$size <- medians$size * fatten
    medians <- transform(medians,
      x = xmin, 
      xend = xmax, 
      y = mode, 
      yend = mode)

    data$linetype <- "blank"
            
    ggplot2:::ggname("geom_hdr", grobTree(
      outliers_grob,
      GeomRect$draw_panel(data, panel_scales, coord),
      GeomSegment$draw_panel(medians, panel_scales, coord)
    ))
  },
  
  draw_legend = function(data, ...)  {
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype))
    ))
  }
)

#' Side by side hdr plots.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_boxplot
#' @inheritParams ggplot2::geom_violin
#' @export
#' @examples
#' \donttest{
#' require(ggplot2)
#' data(diamonds)
#' gghdr(diamonds, color, price)
#' gghdr(diamonds, cut, price, probs=c(50,25,12.5, 6.25, 1), fill=cut) + 
#'   scale_fill_brewer(palette="Set1") + 
#'   scale_colour_brewer(palette="Set1")
#' 
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' 
#' p + geom_hdr()
#' p + geom_hdr(aes(fill=factor(..probs..)), probs=c(.5, .25, .125)) + scale_fill_brewer() 
#' gghdr(mtcars, factor(cyl), mpg,  probs=c(50,25,12.5))
#' 
#' qplot(factor(cyl), mpg, data = mtcars, geom = "hdr")
#' 
#' p + geom_hdr() + geom_jitter(height = 0)
#' p + geom_hdr() + coord_flip()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "hdr") +
#'   coord_flip()
#' 
#' # Add aesthetic mappings
#' # Note that hdrs are automatically dodged when any aesthetic is 
#' # a factor
#' p + geom_hdr(aes(fill = cyl)) 
#' p + geom_hdr(aes(fill = factor(cyl)))
#' p + geom_hdr(aes(fill = factor(vs))) 
#' p + geom_hdr(aes(fill = factor(am))) 
#' 
#' # Set aesthetics to fixed value
#' p + geom_hdr(fill = "grey80", colour = "#3366FF")
#' qplot(factor(cyl), mpg, data = mtcars, geom = "hdr", 
#'   colour = I("#3366FF"))
#' 
#' x <- rnorm(2000)
#' group <- rep(1:20, 100)
#' y <- rep(c(1,2),1000)
#' qplot(y,x,facets=~group, fill=factor(y), geom="hdr") 
#' }
geom_hdr <- function (mapping = NULL, data = NULL, stat = "hdr", position = "dodge", show.legend = NA, inherit.aes = TRUE, width = 0.9, alpha = 0.25,
                       ...) {
  layer(
    geom = GeomHdr, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, ...)
  )
}
