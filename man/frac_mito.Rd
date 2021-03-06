% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc.R
\name{frac_mito}
\alias{frac_mito}
\title{Fraction of a Cell's Expression Level Coming From Mitochondrial Genes}
\usage{
frac_mito(m)
}
\arguments{
\item{m}{matrix of expression values to test}
}
\description{
Often used as a QC parameter for cells in scRNA-seq data. The higher the fraction of mitochondrial expression (i.e. of expression coming from mitochondrial genes), the higher the implied level of stress a cell is in and by consequence the lower the assumed quality. In scRNAseq cancer data, a cutoff of 0.25 is often used although any individual value is always arbitrary and one is better off plotting the data. note: the list of mitochondrial genes are those that start with "MT".
}
