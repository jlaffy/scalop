
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @param m PARAM_DESCRIPTION, Default: NULL
#' @param p PARAM_DESCRIPTION, Default: c(0.01, 0.001, 1e-04)
#' @param center.rows PARAM_DESCRIPTION, Default: TRUE
#' @param lfc PARAM_DESCRIPTION, Default: log2(2L)
#' @param pmethod PARAM_DESCRIPTION, Default: 'BH'
#' @param alternative PARAM_DESCRIPTION, Default: 'greater'
#' @param simplify PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nSigGenes
#' @export 
nSigGenes = function(group = NULL,
                     m = NULL,
                     p = c(0.01, 1e-03, 1e-04),
                     center.rows = TRUE,
                     lfc = log2(2L),
                     pmethod = 'BH',
                     alternative = 'greater',
                     simplify = TRUE) {

    names(p) = paste('Signif', p, sep = '_')

    result = dea(m = m,
                 group = group,
                 p = max(p),
                 lfc = lfc,
                 pmethod = pmethod,
                 alternative = alternative,
                 center.rows = center.rows,
                 return.val = 'p')

    counts = sapply(result, function(rdea) {
                        sapply(p, function(pval) {
                                   sum(rdea <= pval)})},
                        simplify = simplify)

    if (length(counts) == 1) {
        counts = counts[[1]]
    }

    counts
}
