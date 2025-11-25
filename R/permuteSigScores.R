
#' @title Binomial test on matrix
#' @description Performs binomial test on each row (or column) of a logical/binary matrix to test if there are more TRUE values than expected by chance. Tests the alternative hypothesis that the proportion of successes is greater than expected.
#' @param m matrix (logical or binary)
#' @param adjust.method p-value adjustment method. Default: 'fdr'
#' @param MARGIN 1 for rows, 2 for columns. Default: 1
#' @return adjusted p-values
binom.test_matrix = function(m, adjust.method='fdr', MARGIN=1) {
    # H1 = more TRUE (extreme values) than expected
    alternative='greater'
    m = as.matrix(m)
    if (MARGIN == 1) {x = rowSums(m); p = sum(m)/length(m); n = ncol(m)}
    else {x = colSums(m) ; p = x/length(m); n = nrow(m)}
    pvals = sapply(x, function(xi) binom.test(x = xi, n = n, p = p, alternative = alternative)$p.value, simplify = F)
    pvals = unlist(pvals)
    stats::p.adjust(pvals, method=adjust.method)
}

#' @title Permute matrix by shuffling across cells
#' @description Shuffles expression values across cells independently for each gene (row). Preserves gene identities and expression distributions while breaking co-expression patterns within cells. Used to generate null distributions for permutation testing.
#' @param m matrix (genes by cells)
#' @param MARGIN 1 for rows (shuffles across cells for each gene), 2 for columns. Default: 1
#' @return permuted matrix with same dimensions and names as input
permute_matrix = function(m, MARGIN = 1) {
    t(apply(m, MARGIN, sample, replace = FALSE)) %>%
        scalop::setColNames(., colnames(m)) %>%
        scalop::setRowNames(., rownames(m))
}

#' @title Permutation-based signature scoring with statistical testing
#' @description Computes signature scores for observed data and compares against N permuted null distributions. For each permutation, gene expression values are shuffled across cells (independently for each gene) to break co-expression patterns while preserving gene identities and expression distributions. Identifies cells with significantly high (or low) signature scores based on permutation testing. Scores are considered significant if they exceed mean + 2*SD (or fall below mean - 2*SD) of the permuted distribution.
#' @param m expression matrix (genes by cells)
#' @param sigs list of gene signatures
#' @param center.fun optional centering function to apply to matrix before scoring. Default: NULL
#' @param center.rows whether to center rows in sigScores. Default: TRUE
#' @param expr.bin.m optional expression bin matrix for sigScores. Default: NULL
#' @param N number of permutations to generate null distribution. Default: 50
#' @param alternative test direction: 'greater' (high scores), 'two.sided' (extreme scores in either direction), or 'less' (low scores). Default: 'greater'
#' @param ... additional arguments passed to sigScores
#' @return data frame with columns: id (cell), sig (signature name), score (observed score), fdr (adjusted p-value)
#' @export
permuteSigScores = function(m, 
			    sigs, 
			    center.fun=NULL, 
			    center.rows=TRUE,
			    expr.bin.m=NULL,
			    N=50, 
			    alternative=c('greater','two.sided','less'),
			    ...) {

    # N specifies number of different distributions to generate

    if (!is.null(center.fun)) {mc=center.fun(m); expr.bin.m=m; center.rows=F} 
    else {mc=m}
    
    scores0 = scalop::sigScores(m=mc, sigs=sigs, center.rows=center.rows, expr.bin.m=expr.bin.m, ...)
    
    start.time = Sys.time()
    res = list()

    alternative=match.arg(alternative)
    if (alternative=='two.sided') {
	    funi = function(orig, perm) (orig > mean(perm) + 2*sd(perm)) | (orig < mean(perm) - 2*sd(perm))
    } else if (alternative=='greater') {
	    funi = function(orig, perm) orig > mean(perm) + 2*sd(perm)
    } else {
	    funi = function(orig, perm) orig < mean(perm) - 2*sd(perm)
    }
    
    for (i in 1:N) {
        mperm = permute_matrix(m, MARGIN = 1)
    	if (!is.null(center.fun)) {mc=center.fun(mperm); expr.bin.m=mperm; center.rows=F} 
    	else {mc=mperm}
    	scores = scalop::sigScores(m=mc, sigs=sigs, center.rows=center.rows, expr.bin.m=expr.bin.m, ...)
        resi = Map(funi, orig = scores0, perm = scores) %>%
            do.call(cbind.data.frame, .) %>%
            scalop::setRowNames(., rownames(scores)) %>%
            scalop::setColNames(., colnames(scores))
        res <- c(res, list(resi))
    }

    end.time = Sys.time()
    print(round(end.time - start.time, 5))

    # rearrange <res> to be one matrix per state rather than one matrix per permutation
    res = sapply(colnames(scores0), function(state) {
                                 setRowNames(do.call(cbind, sapply(res, `[`, state)), rownames(scores0))},
                                 simplify = F) 

    pvals = do.call(cbind.data.frame, sapply(res, binom.test_matrix, simplify = F)) %>%
        setRowNames(., rownames(scores0)) %>%
	    tibble::rownames_to_column('id') %>%
	    reshape2::melt() %>%
	    dplyr::rename(fdr=value,sig=variable) %>%
	    dplyr::mutate(sig=as.character(sig))

    scores0 = scores0 %>% 
	    as.data.frame %>% 
	    tibble::rownames_to_column('id') %>%
	    reshape2::melt() %>%
	    dplyr::rename(score=value,sig=variable) %>%
	    dplyr::mutate(sig=as.character(sig))
    
    full_join(scores0, pvals)
}
