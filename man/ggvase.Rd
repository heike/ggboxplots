\name{ggvase}
\alias{ggvase}
\title{Vase plots.}
\usage{
  ggvase(data, x, y, bandwidth, ...)
}
\arguments{
  \item{data}{dataset}

  \item{x}{factor variable}

  \item{y}{continuous variable}

  \item{bandwidth}{value for the bandwidth of the kernel
  smoother}

  \item{...}{other parameters}
}
\value{
  ggplot2 layer of vases
}
\description{
  Create a vase plot within the ggplot2 framework
}
\details{
  Vase plots have been introduced by Yoav (1988). The idea
  of a vase plot is to represent the kernel density
  estimate instead of the box for the inter-quartile range
  of a regular boxplot. A bandwidth parameter is used to
  control for smoothness of the curve.
}
\examples{
require(ggplot2)
data(diamonds)
ggvase(diamonds, color, price, bandwidth=500)
ggvase(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
}
\author{
  Heike Hofmann
}
\references{
  Benjamini Y (1988). "Opening the Box of a Boxplot." _The
  American Statistician_, pp. 257-262.
}

