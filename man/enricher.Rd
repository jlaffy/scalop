% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enricher.R, R/old-enricher.R
\name{enricher}
\alias{enricher}
\title{FUNCTION_TITLE}
\usage{
enricher(
  my_gene_sets,
  my_universe,
  p.adj = 0.01,
  term2gene = NULL,
  category = c("C2", "C5"),
  subcategory = NULL
)

enricher(
  my_gene_sets,
  my_universe,
  p.adj = 0.01,
  term2gene = NULL,
  category = c("C2", "C5"),
  subcategory = NULL
)
}
\arguments{
\item{my_gene_sets}{PARAM_DESCRIPTION}

\item{my_universe}{PARAM_DESCRIPTION}

\item{p.adj}{PARAM_DESCRIPTION, Default: 0.01}

\item{term2gene}{PARAM_DESCRIPTION, Default: NULL}

\item{category}{PARAM_DESCRIPTION, Default: c("C2", "C5")}

\item{subcategory}{PARAM_DESCRIPTION, Default: NULL}

\item{ref_gene_sets}{PARAM_DESCRIPTION}

\item{minGSSize}{PARAM_DESCRIPTION, Default: 10}

\item{maxGSSize}{PARAM_DESCRIPTION, Default: 500}

\item{pAdjustMethod}{PARAM_DESCRIPTION, Default: 'BH'}

\item{pvalueCutoff}{PARAM_DESCRIPTION, Default: 0.05}

\item{qvalueCutoff}{PARAM_DESCRIPTION, Default: 0.2}
}
\value{
OUTPUT_DESCRIPTION

OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION

FUNCTION_DESCRIPTION
}
\details{
DETAILS

DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[limma]{alias2Symbol}}
 \code{\link[clusterProfiler]{enricher}}

\code{\link[msigdbr]{msigdbr}}
 \code{\link[clusterProfiler]{enricher}}
}
