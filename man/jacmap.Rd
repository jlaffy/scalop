% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jacmap.R
\name{jacmap}
\alias{jacmap}
\title{Convert a list to a jaccard matrix and plot with `scalop::gmap`}
\usage{
jacmap(
  L = NULL,
  L2 = NULL,
  m = NULL,
  ratio = 1,
  limits = c(0, 1),
  tile.col = "black",
  tile.size = 0.1,
  cols = c("white", "darkred"),
  num = FALSE,
  angle = T,
  ...,
  tiletext = FALSE,
  dcp = 1
)
}
\arguments{
\item{L}{list of character vectors to compute jaccard indexes between.}

\item{ratio}{aspect ratio. Default: 1}

\item{limits}{colour key limits. Default: c(0, 1)}

\item{tile.col}{draw lines between tiles. Default: 'black'}

\item{tile.size}{draw lines between tiles. Default: 0.1}

\item{cols}{colours for key. Default: c("white", "darkred")}

\item{num}{numeric (TRUE) or character (FALSE) x- and y- axes labels. Default: FALSE}

\item{angle}{angle x-axis labels. Default: T}

\item{...}{other arguments passed to `scalop::gmap`}

\item{tiletext}{write jaccard indexes on tiles? Default: FALSE}

\item{dcp}{relevant if tiletext is TRUE. How many decimal places to show? Default: 1}
}
\value{
ggplot object
}
\description{
Convert a list to a jaccard matrix and plot with `scalop::gmap`
}
\seealso{
\code{\link[reshape2]{melt}}
 \code{\link[ggplot2]{geom_label}}
}
