
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param adjust.method PARAM_DESCRIPTION, Default: 'fdr'
#' @param MARGIN PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stats]{p.adjust}}
#' @rdname binom.test_matrix
#' @export 
#' @importFrom stats p.adjust
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param MARGIN PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[scalop]{setColNames}}, \code{\link[scalop]{setRowNames}}
#' @rdname permute_matrix
#' @export 
#' @importFrom scalop setColNames setRowNames
permute_matrix = function(m, MARGIN = 1) {
    t(apply(m, MARGIN, sample, replace = FALSE)) %>%
        scalop::setColNames(., colnames(m)) %>%
        scalop::setRowNames(., rownames(m))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param sigs PARAM_DESCRIPTION
#' @param center.fun PARAM_DESCRIPTION, Default: NULL
#' @param center.rows PARAM_DESCRIPTION, Default: TRUE
#' @param expr.bin.m PARAM_DESCRIPTION, Default: NULL
#' @param N PARAM_DESCRIPTION, Default: 50
#' @param alternative PARAM_DESCRIPTION, Default: c("greater", "two.sided", "less")
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[scalop]{sigScores}}, \code{\link[scalop]{setRowNames}}, \code{\link[scalop]{setColNames}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}
#' @rdname permuteSigScores
#' @export 
#' @importFrom scalop sigScores setRowNames setColNames
#' @importFrom tibble rownames_to_column
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate
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

