% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_template.R
\name{add_tcc_template}
\alias{add_tcc_template}
\title{Build the tcc template to be used for a quick setup for tcc and easy understanding of tcc.}
\usage{
add_tcc_template(
  tcc_template = NULL,
  tcc_template_name,
  theta,
  tcc_lower,
  tcc_upper
)
}
\arguments{
\item{tcc_template}{the tcc template}

\item{tcc_template_name}{the name of this tcc template}

\item{theta}{the thetas}

\item{tcc_lower}{the tcc lower bound}

\item{tcc_upper}{the tcc upper bound}
}
\value{
a tibble of tcc template
}
\description{
each tcc template contains a name and a tibble contains the columns THETA, TCC_LOWER,
and TCC_UPPER. If the tcc template name has existed, it will be overwritten.
}
