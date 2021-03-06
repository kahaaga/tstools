% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_outliers.R
\name{remove_outliers}
\alias{remove_outliers}
\title{Removes outliers from a vector (values outside 1.5 * IQR).}
\usage{
remove_outliers(v)
}
\arguments{
\item{v}{A scalar valued vector.}
}
\value{
The vector 'v' with the outliers removed.
}
\description{
Removes outliers from a vector (values outside 1.5 * IQR).
}
