% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msigdb.R
\name{msigdb}
\alias{msigdb}
\title{FUNCTION_TITLE}
\usage{
msigdb(
  category = c("H", "C2", "C5"),
  subcategory = NULL,
  exclude.subcategory = "HPO",
  split.by.subcategory = F,
  species = "Homo sapiens",
  annotation = c("gene_symbol", "entrez_gene"),
  return.dataframe = F
)
}
\arguments{
\item{category}{PARAM_DESCRIPTION, Default: c("H", "C2", "C5")}

\item{subcategory}{PARAM_DESCRIPTION, Default: NULL}

\item{exclude.subcategory}{PARAM_DESCRIPTION, Default: 'HPO'}

\item{split.by.subcategory}{PARAM_DESCRIPTION, Default: F}

\item{species}{PARAM_DESCRIPTION, Default: 'Homo sapiens'}

\item{annotation}{PARAM_DESCRIPTION, Default: c("gene_symbol", "entrez_gene")}

\item{return.dataframe}{PARAM_DESCRIPTION, Default: F}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[msigdbr]{msigdbr}}
 \code{\link[dplyr]{filter}}
}
