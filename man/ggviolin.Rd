\name{ggviolin}
\alias{ggviolin}
\title{Violin plots.}
\usage{
  ggviolin(data, x, y, bandwidth, alpha = 0.5,
    fill = "grey60", ...)
}
\arguments{
  \item{data}{dataset}

  \item{x}{factor variable}

  \item{y}{values}

  \item{bandwidth}{value for kernel density}

  \item{alpha}{parameter for alpha blending}

  \item{fill}{color or symbol to fill in violins.}

  \item{...}{other parameters}
}
\value{
  ggplot2 layer of violins
}
\description{
  Create a violin plot within the ggplot2 framework. The
  code for calculating violins is based on the
  \code{vioplot 0.2} package.
}
\details{
  Violin plots have been introduced by Hintze and Nelson
  (1998). Additionally to the boxes of a boxplot, two
  density curves are plotted opposing each other, and the
  space between them is filled to create the shape of a
  'violin'. As for any density estimation, a bandwidth
  parameter is used to control smoothness.
}
\examples{
require(ggplot2)
data(diamonds)
ggviolin(diamonds, color, price, bandwidth=500)
ggviolin(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
}
\author{
  Heike Hofmann
}
\references{
  Hintze JL and Nelson RD (1998). "Violin Plots: A Box
  Plot-Density Trace Synergism." _The American
  Statistician_, *52*(2), pp. 181-84.
}

