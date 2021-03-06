% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc.R
\name{aggr_gene_expr}
\alias{aggr_gene_expr}
\title{log-Average expression of genes across all cells}
\usage{
aggr_gene_expr(m, na.rm = T, isBulk = FALSE)
}
\arguments{
\item{m}{matrix of expression values to test}
}
\description{
Often used as a QC parameter for genes in scRNA-seq data, usually once the LQ cells have been removed. The matrix is converted to TPM space from logTPM (more specifically, from log2(TPM/10 + 1)), gene averages are calculated and finally the values are converted back to log2(TPM + 1), without dividing by 10 since the data is now simulated bulk.
}
