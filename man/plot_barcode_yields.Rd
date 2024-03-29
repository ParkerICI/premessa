% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/debarcoder_plotting.R
\name{plot_barcode_yields}
\alias{plot_barcode_yields}
\title{Plotting sample yields after debarcoding}
\usage{
plot_barcode_yields(
  bc.results,
  sep.threshold,
  mahal.threshold = NULL,
  mahal.dist = NULL
)
}
\arguments{
\item{bc.results}{The debarcoding results returned from \code{debarcode_data}}

\item{mahal.threshold}{If not \code{NULL} The threshold value for the Mahalanobis distance between each
event and the centroid of the sample the event has been assigned to. Only events with distance lower
than this threshold are assigned to the sample.}

\item{mahal.dist}{A vector of Mahalnobis distances, as returned by \code{get_mahalanobis_distance}. Only
used if \code{mahal.threshold} is not \code{NULL}}
}
\value{
Returns a ggplot2 object
}
\description{
Plotting sample yields after debarcoding
}
