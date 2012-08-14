\name{calc_violin}
\alias{calc_violin}
\title{Calculations for violin plots.}
\usage{
  calc_violin(x, at, h = NA, range = 1.5, wex = 1)
}
\arguments{
  \item{x}{continuous variable}

  \item{at}{position of each violin. Defaults to 1:n}

  \item{h}{bandwidth for kernel density}

  \item{range}{numerical value to calculate the upper/lower
  adjacent values}

  \item{wex}{relative expansion of the violin}
}
\value{
  list of numeric descriptions of body and outliers
}
\description{
  Extract the values for plotting kernel density according
  to package vioplot copied (except for return statement)
  verbatim from function vioplot, package vioplot v0.2
}

