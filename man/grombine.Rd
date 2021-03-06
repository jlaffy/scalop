% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grombine.R
\name{grombine}
\alias{grombine}
\title{Combine ggplot grobs or plots}
\usage{
grombine(..., by = "x", size = "last", draw = T)
}
\arguments{
\item{...}{plots/grobs to combine. A mix of the two is fine.}

\item{by}{axis to combine plots by. Default: 'x'}

\item{size}{which plot's dimensions to prioritise. One of 'first' or 'last'. Default: 'last'}

\item{draw}{logical to return plot instead of grob. Default: T}
}
\value{
ggplot plot or grob object, with all objects in ... combined.
}
\description{
combine ggplot grobs or plots
}
\seealso{
\code{\link[ggplot2]{ggplotGrob}}
 \code{\link[grid]{grid.draw}}
}
