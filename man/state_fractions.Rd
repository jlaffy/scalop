% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state_fractions.R
\name{state_fractions}
\alias{state_fractions}
\title{State Fractions (by group)}
\usage{
state_fractions(scores, groups = NULL, min = NULL, diff = NULL)
}
\arguments{
\item{scores}{a data.frame object of cell signature scores (cells X signatures/states)}

\item{groups}{should fractions be computed separately for groups, e.g. samples? a list of observations by group. Default: NULL}

\item{min}{minimum value required for a row to be assigned to a column. Default: NULL}

\item{diff}{minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: NULL}
}
\value{
a numeric or data.frame object with states as (col)names and if data.frame, groups as rownames.
}
\description{
Get the fraction of cells in each state, by group if <groups> provided. Here, the rows in <scores> are interpreted as cells and the columns as their state scores, but more generally the function assigns each row to its maximum column (state) and subsequently counts the number of observations per state where if <groups> provided then the observations are first subsetted by group.
}
\seealso{
\code{\link[stats]{setNames}}
}
