.dea = function(m,
                x,
                is.log = T,
                fc = 2L,
                p = 0.05,
                p.adjust.method = 'BH',
                sortby = 'fc',
                val = NULL) {

    if (is.log & !is.null(fc)) fc = log2(fc)
    if (!is.factor(x)) x = makefac(x, cols = colnames(m))

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

#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param m numeric matrix. The matrix should not contain NA values.
#' @param groups the group(s) to be tested. Each group should be a character vector or a two-level factor in which the first corresponds to the group. Multiple groups must be supplied as a list.
#' @param is.log is the data in log2 space? Fold change cutoff and calculation are adjusted accordingly. Default: T
#' @param fc fold change cutoff; numeric value or NULL. Default: 2
#' @param p p-value cutoff; numeric value between 0 and 1 or NULL. Default: 0.05
#' @param p.adjust.method correction method for multiple testing or 'none' if not desired. See stats::p.adjust.methods for options. Default: 'BH'
#' @param sortby sort genes by 'fc', 'p' or NULL. Default: 'fc'
#' @param val return a named numeric vector of 'fc' or 'p' values. If NULL, returns the full dataframe. Default: NULL 
#' @return a numeric vector of gene fold changes or p-values or a dataframe with both.
#' @seealso 
#'  \code{\link[stats]{character(0)}}
#' @rdname dea
#' @export 
#' @importFrom stats p.adjust.methods
dea = function(m,
               groups,
               is.log = T,
               fc = 2L,
               p = 0.05,
               p.adjust.method = 'BH',
               sortby = 'fc',
               val = NULL) {

    if (is.character(groups) | is.factor(groups)) x = list(x)
    else stopifnot(is.list(groups))
    if (is.null(names(groups)) && length(groups) > 1) names(groups) = 1:length(groups)
    Args = mget(ls(), envir = environment())[names(Args) != 'x']
    Map(.dea, x = groups, MoreArgs = Args)
}
