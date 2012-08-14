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
  Need the reference to the Benjamini paper.
}
\examples{
data(diamonds)
ggvase(diamonds, color, price, bandwidth=500)
ggvase(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
}
\author{
  Heike Hofmann
}

