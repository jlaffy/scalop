#' @title Quality control statistics for single cell RNA seq data
#' @description Quality control statistics for single cell RNA seq data
#' @param m expression matrix of genes by cells in non-log2 space e.g. CPM or TPM.
#' @param mito.genes character vector of mitochondrial gene names. Default: rownames(m)[startsWith(rownames(m), "MT-")]
#' @return dataframe with columns: cell, detected_genes, housekeeping_expr, mito_Frac 
#' @seealso 
#'  \code{\link[plyr]{mapvalues}}
#' @rdname ohmyqc
#' @export 
#' @importFrom plyr mapvalues
ohmyqc = function(m,
                  mito.genes = rownames(m)[startsWith(rownames(m), "MT-")]) {

    # cell complexity (number of detected genes)
    detected_genes = coldetected(m)

    # housekeeping expression score
    housekeeping_expr = expr_housekeeping(logtpm(m, bulk = FALSE))
    
    # fraction of counts coming from mitochondrial genes
    if (length(mito.genes) != 0) {
        mito_frac = colSums(m[mito.genes,])/colSums(m)
        mito_frac = plyr::mapvalues(mito_frac, NaN, 0)
    } else {
        mito_frac = rep(NA, ncol(m))
    }

    dat = data.frame(cell = colnames(m), 
                     detected_genes = detected_genes,
                     housekeeping_expr = housekeeping_expr,
                     mito_frac = mito_frac,
                     stringsAsFactors = FALSE)
    class(dat) <- c('ohmyqc', class(dat))
    dat
}
