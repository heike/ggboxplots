\name{calc_violin}
\alias{calc_violin}
\title{Calculations for violin plots.}
\usage{
  calc_violin(x, at, h = NA, range = 1.5, wex = 1)
}
\arguments{
  \item{x}{continuous variable}

  \item{at}{}

  \item{h}{bandwidth for kernel density}

  \item{range}{}

  \item{wex}{}
}
\value{
  list of numeric descriptions of body and outliers
}
\description{
  Extract the values for plotting kernel density according
  to package vioplot copied (except for return statement)
  verbatim from function vioplot, package vioplot v0.2
}

