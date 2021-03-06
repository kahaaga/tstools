% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw-random-series.R
\name{draw_random_data}
\alias{draw_random_data}
\title{Draws random datagiven a vector 'data' and its associated uncertainties,
either in the form of 'sigmas' or a set of 'lower.bounds' and 'upper.bounds'.
If 'sigmas' is provided, draw from a Gaussian centered on the data. If bounds
are provided, draw from a Gaussian truncated at the lower and upper bounds.}
\usage{
draw_random_data(data, sigmas = NULL, n.sigma = 2, n.replicates = 1,
  lower.bounds = NULL, upper.bounds = NULL)
}
\arguments{
\item{data}{A vector of data.}

\item{sigmas}{A vector of 1 sigma uncertainties associated with the data.}

\item{n.sigma}{How many sigmas should be allowed?}

\item{n.replicates}{The number of random data series to draw.}

\item{lower.bounds}{A vector of lower bounds for the data.}

\item{upper.bounds}{A vector of upper bounds for the data.}
}
\value{
A matrix of randomly drawn data series.
}
\description{
Draws random datagiven a vector 'data' and its associated uncertainties,
either in the form of 'sigmas' or a set of 'lower.bounds' and 'upper.bounds'.
If 'sigmas' is provided, draw from a Gaussian centered on the data. If bounds
are provided, draw from a Gaussian truncated at the lower and upper bounds.
}
