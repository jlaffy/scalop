

#' @title Filter Out Lowly-Correlated Genes from Signature(s)  
#' @description This function removes signature genes whose expression level is poorly correlated to the signature score. This is relevant if the signatures themselves were defined from a different dataset, as might be the case in applying single-cell signatures to bulk, or in moving between datasets generated with different protocols (e.g. single-nuclei RNA seq. vs single-cell RNA seq.).
#' @param m expression matrix of genes X cells to score. Not centered.
#' @param sigs list of gene signatures to be refined.
#' @param filter.threshold minimum pearson's r below which genes are filtered out of the signature. Default: 0.4
#' @return a filtered list of signatures, whose genes' expression levels all had correlation values to the signature score above or equal to <filter.threshold>.
#' @rdname filter_signatures
#' @export 
filter_signatures = function(m, sigs, filter.threshold = 0.4) {
# score observations (cells / samples)
# remove genes from signature that are lowly correlated to score
# rescore observations
# this is the refined signature

    scores = as.data.frame(sigScores(m, sigs))
    nams = names(sigs)
    orig = sigs
    sigs = sapply(sigs, function(genes) genes[genes %in% rownames(m)])
    sigs = sapply(1:ncol(scores), function(i) {
                      sapply(sigs[[i]], function(gene) {
                                 cor(as.numeric(m[gene, ]), scores[[i]])})})
    names(sigs) = nams
    sigs = sapply(sigs, function(genecor) {
                      names(genecor)[genecor >= filter.threshold]},
                      simplify = F)
    sigs
#    rem.sigs = sigs
#
#    scores = as.data.frame(sigScores(m, sigs))
#    nams = names(sigs)
#    Call = quote(sapply(rownames(m), function(gene) {
#                            cor(as.numeric(m[gene, ]), scores[[i]])}))
#
#    add.sigs = sapply(1:ncol(scores), function(i) eval(Call), simplify = F)
#    names(add.sigs) = nams
#    add.sigs = sapply(add.sigs, function(genecor) {
#                          names(genecor)[genecor >= add.threshold]},
#                          simplify = F)
#
#    sigs = Map(function(x, y) unique(c(x, y)), rem.sigs, add.sigs)
#    list(orig = orig, filtered = rem.sigs, added = add.sigs, final = sigs)
}
