#' A package for creating common variations of boxplots
#'
#' This package brings a set of variations of boxplots to the \code{ggplot2}.framework.
#' Implemented are vase plots, violin plots, and high density 
#' region plots. 
#' See \code{\link{ggvase}, \link{ggviolin}} and \code{\link{gghdr}} 
#'
#' @docType package
#' @name ggboxplots 
#' @references benjamini hintze hyndman
#' @examples
#' require(ggplot2)
#' ggviolin(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
#' gghdr(diamonds, cut, price, probs=c(50,25,12.5, 6.25), fill=cut) + 
#'   scale_fill_brewer(palette="Set1") +
#'   scale_colour_brewer(palette="Set1")
#' ggvase(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
NULL