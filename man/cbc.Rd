% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbc.R
\name{cbc}
\alias{cbc}
\title{Solve a linear program using the cbc solver}
\usage{
cbc(lp, timeout = 60, gap = 0, verbose = T, ...)
}
\arguments{
\item{lp}{An R object that encapsulates the data used to specify a
linear program}

\item{timeout}{The number of CPU seconds before the solver must timeout}

\item{gap}{Absolute MIP gap}

\item{verbose}{Verbosity level}
}
\value{
Updated lp object
}
\description{
Solve a linear program using the cbc solver
}
