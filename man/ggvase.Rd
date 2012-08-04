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

  \item{...}{other parameters}
}
\value{
  ggplot2 layer of vases
}
\description{
  Create a vase plot within the ggplot2 framework
}
\examples{
data(diamonds)
ggvase(diamonds, color, price, bandwidth=500)
ggvase(diamonds, cut, price, bandwidth=500)
}
\author{
  Heike Hofmann
}

