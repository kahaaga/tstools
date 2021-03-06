% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw-random-series.R
\name{datamodel_truncated}
\alias{datamodel_truncated}
\title{Draws random data according to a Gaussian with standard deviation
n.sigma centered at the original data and truncated at the
lower.bounds and upper.bounds.}
\usage{
datamodel_truncated(data, lower.bounds, upper.bounds, n.sigma)
}
\arguments{
\item{data}{A vector of data.}

\item{lower.bounds}{A vector of lower bounds.}

\item{upper.bounds}{A vector of upper bounds.}

\item{n.sigma}{The number of standard deviations for the Gaussian.}
}
\description{
Draws random data according to a Gaussian with standard deviation
n.sigma centered at the original data and truncated at the
lower.bounds and upper.bounds.
}
