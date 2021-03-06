% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxcol_strict.R
\name{maxcol_strict}
\alias{maxcol_strict}
\title{Strict Column Assign}
\usage{
maxcol_strict(mat, min = NULL, diff = NULL, splitByCol = FALSE)
}
\arguments{
\item{mat}{a matrix}

\item{min}{minimum value required for a row to be assigned to a column}

\item{diff}{minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column.}

\item{splitByCol}{a boolean value indicating whether the resulting vector should be split by column variable. Default = FALSE}
}
\value{
a vector of the rownames whose values were highest in that column, or if splitByCol is TRUE, a list.
}
\description{
For each column, returns a vector of the rownames whose values were highest. The "snobby" part comes if the user provides values for min and diff, which require that a row's max value be higher than or equal to <min> and with a difference greater than or equal to <diff> in order to be assigned to a column. Rows that do not pass these criteria are ignored.
}
