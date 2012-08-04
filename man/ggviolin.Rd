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
  Create a violin plot within the ggplot2 framework
}
\details{
  Violin plots have been introduced by Hintze and Nelson
  (1998).
}
\examples{
data(diamonds)
ggviolin(diamonds, color, price, bandwidth=500)
ggviolin(diamonds, cut, price, bandwidth=300, fill=cut) + scale_fill_brewer(palette="Set1")
}
\author{
  Heike Hofmann
}
\references{
  Hintze, Jerry L., and Ray D. Nelson. 1998. "Violin Plots:
  A Box Plot-Density Trace Synergism." The American
  Statistician 52(2):181-84
}

