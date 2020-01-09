
#' @title Aggregate `scalop::dea` output for one group against many in turn
#' @param L.dea list of gene-named-numeric log2-fold change values
#' @param coverage prioritise genes that appear as DE in at least this fraction of the dea result vectors in 'L.dea'. Default: 0.5
#' @param return.avg.lfc logical; if FALSE (default) return gene names only (ordered by average log2 fold change). If TRUE, return average values. Default: FALSE
#' @rdname combine_group2_dea
#' @export
combine_group2_dea = function(L.dea, coverage = 0.5, return.avg.lfc = FALSE) {
    df = scalop::ldcast(L.dea)
    df = as.matrix(df)
    avg.dea = rowMeans(df, na.rm = T)
    coverage.dea = as.numeric(rowMeans(!is.na(df)) >= coverage)
    ord = order(coverage.dea, avg.dea, decreasing = TRUE)
    if (return.avg.lfc) {
        return(avg.dea[ord])
    }
    rownames(df)[ord]
}
