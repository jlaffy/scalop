
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
