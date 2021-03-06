% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_fun.R
\name{test_expected_raw_score}
\alias{test_expected_raw_score}
\title{Calculate expected raw scores for 3PL or/and GPC items}
\usage{
test_expected_raw_score(theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7)
}
\arguments{
\item{theta}{A vector of ability values}

\item{IRT.par}{A matrix of IRT parameters. This should have the columns required by the IRT model of choice.}

\item{IRT.model}{A vector of character strings that specify the IRT model for each item.
This can also be a single value, which will be recycled.}

\item{IRT.scale}{A vector of scaling parameters used in the IRT models.
This can also be a single value, which will be recycled.}

\item{is_percent_scale}{if TRUE, the expected percent score for the given thetas will be returned, which is the tcc.}
}
\value{
An vector of expected percent score for all items
}
\description{
Calculate expected raw scores for 3PL or/and GPC items
}
