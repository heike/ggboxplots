#' A package for creating common variations of boxplots
#'
#' Boxplot variations, such as vase plots, violin plots, and high density 
#' region plots. 
#' See \code{\link{ggvase}, \link{ggviolin}} and \code{\link{gghdr}} 
#'
#' @docType package
#' @bibliography ggboxplots/inst/references.bib
#' @name ggboxplots 
#' @cite benjamini hintze hyndman
#' @examples
#' require(ggplot2)
#' ggviolin(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
#' gghdr(diamonds, cut, price, probs=c(50,25,12.5, 6.25), fill=cut) + 
#'   scale_fill_brewer(palette="Set1") +
#'   scale_colour_brewer(palette="Set1")
#' ggvase(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
NULL