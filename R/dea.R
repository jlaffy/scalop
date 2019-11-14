
.as_fac = function(x, cols) {
    factor(as.numeric(!cols %in% x) + 1)
}

#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param x the group to be tested. This can be supplied as a character vector, or as a factor containing two levels where the first corresponds to the group.
#' @param m numeric matrix. The matrix should not contain NA values.
#' @param is.log is the data in log2 space? Fold change cutoff and calculation are adjusted accordingly. Default: T
#' @param fc fold change cutoff. Set to NULL to not filter genes based on fold change. Default: 2
#' @param p p value cutoff. Set to NULL to not filter genes based on p-values. Default: 0.05
#' @param p.adjust.method correction for multiple testing? One of stats::p.adjust.methods, or 'none' if not desired. Default: 'BH'
#' @param sortby sort genes by 'fc', 'p' or NULL. Default: 'fc'
#' @param val return a named numeric vector of 'fc' or 'p' values. If NULL, returns the full dataframe. Default: NULL 
#' @return a numeric vector of gene fold changes or p-values or a dataframe with both.
#' @seealso 
#'  \code{\link[stats]{character(0)}}
#' @rdname dea
#' @export 
#' @importFrom stats p.adjust.methods
dea = function(x,
               m,
               is.log = T,
               fc = 2L,
               p = 0.05,
               p.adjust.method = 'BH',
               sortby = 'fc',
               val = NULL) {

    if (is.log & !is.null(fc)) fc = log2(fc)
    if (!is.factor(x)) x = .as_fac(x, cols = colnames(m))

    res = genefilter::rowttests(m, fac = x)[, c("dm", "p.value")]
    res = tibble::rownames_to_column(res, 'gene')
    colnames(res)[2] = 'fc'

    if (!is.null(fc)) res = dplyr::filter(res, fc >= fc)
    res = dplyr::mutate(res, p.value = stats::p.adjust(p.value, method = p.adjust.method))
    if (!is.null(p)) res = dplyr::filter(res, p.value <= p)

    if (sortby == 'fc') res = dplyr::arrange(res, desc(fc))
    else if (sortby == 'p') res = dplyr::arrange(res, p.value)
    else stop('<sortby> not recognised.')

    if (is.null(val)) return(res)
    else if (val == 'p') return(stats::setNames(res$p.value, res$gene))
    else if (val != 'fc') warning('<val> not recognised. Reverting to default: "fc"')
    stats::setNames(res$fc, res$gene)
}
