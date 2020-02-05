
#' @title Fraction of a Cell's Expression Level Coming From Mitochondrial Genes 
#' @description Often used as a QC parameter for cells in scRNA-seq data. The higher the fraction of mitochondrial expression (i.e. of expression coming from mitochondrial genes), the higher the implied level of stress a cell is in and by consequence the lower the assumed quality. In scRNAseq cancer data, a cutoff of 0.25 is often used although any individual value is always arbitrary and one is better off plotting the data. note: the list of mitochondrial genes are those that start with "MT".
#' @param m matrix of expression values to test
#' @export
#' @rdname frac_mito
frac_mito = function(m) {
    genes = rownames(m)[startsWith(rownames(m), "MT")]
    M = unlogtpm(m)
    colSums(m[genes, ])/colSums(m)
}


#' @title Expression level of Housekeeping Genes 
#' @description Often used as a QC parameter for cells in scRNA-seq data. The lower the expression level of housekeeping the genes, the more DNA contamination it suggests for the sample. In scRNAseq cancer data, this value is also used as a cutoff: e.g. a logTPM value >= 2.5 in order for a cell to be retained as HQ and with minimal DNA contamination. note: the list of housekeeping genes is taken from Normal_Markers$Housekeeping
#' @param m matrix of expression values to test
#' @export
#' @rdname expr_housekeeping
expr_housekeeping = function(m, na.rm = T) {
    genes = scalop::Markers_Normal$Housekeeping
    genes = genes[genes %in% rownames(m)]
    logtpm(colMeans(unlogtpm(m[genes, ]), na.rm = na.rm), bulk = T)
}



#' @title log-Average expression of genes across all cells
#' @description Often used as a QC parameter for genes in scRNA-seq data, usually once the LQ cells have been removed. The matrix is converted to TPM space from logTPM (more specifically, from log2(TPM/10 + 1)), gene averages are calculated and finally the values are converted back to log2(TPM + 1), without dividing by 10 since the data is now simulated bulk.
#' @param m matrix of expression values to test
#' @export
#' @rdname aggr_gene_expr
aggr_gene_expr = function(m, na.rm = T, isBulk = FALSE) {
    logtpm(rowMeans(unlogtpm(m, bulk = isBulk), na.rm = na.rm), bulk = TRUE)
}
