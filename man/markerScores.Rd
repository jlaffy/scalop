% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigScores.R
\name{markerScores}
\alias{markerScores}
\title{Score a Matrix with Marker Gene Sets Of Normal Cell Types}
\usage{
markerScores(m, ...)
}
\arguments{
\item{m}{a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.}

\item{...}{other arguments passed to scalop::sigScores, although none are required.}
}
\value{
a dataframe of cell/sample scores. Each column describes scores for a different Marker Gene Set.
}
\description{
Score a Matrix with Marker Gene Sets Of Normal Cell Types. Wrapper around scalop::sigScores. Please see `?scalop::sigScores` for more details
}
\details{
the gene sets can be seen in Markers_Normal
}
