\name{gghdr}
\alias{gghdr}
\title{High Density Region plots.}
\usage{
  gghdr(data, x, y, probs = c(90, 50, 25))
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
  Create a high density region boxplots (Hyndman, 1996)
  within the ggplot2 framework. The code is based on the
  \code{hdrcde} package
}
\examples{
data(diamonds)
gghdr(diamonds, color, price)
gghdr(diamonds, cut, price, probs=c(50,25,12.5, 6.25)) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1")
}
\author{
  Heike Hofmann
}
\references{
  Hyndman, R.J. (1996) Computing and graphing highest
  density regions American Statistician, 50, 120-126.
}

