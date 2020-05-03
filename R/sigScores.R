
#' @title Filter genes in sigs according to reference 
#' @description Filter genes in sigs according to reference 
#' @param sigs a character vector of genes or list of character vectors to filter 
#' @param ref reference genes to filter sigs according to
#' @param conserved the minimum allowed fraction of genes retained after filtering. Default: 0.7
#' @return a filtered list of sigs. Returns NULL if no genes in any sigs are left. sigs with less than <conserved> fraction of genes retained will be removed. Returns input <sigs> if no genes are missing.
#' @rdname filter_sigs
#' @export 
filter_sigs = function(sigs, ref, conserved = 0.7) {
    # no need for filtering
    if (all(unlist(sigs) %in% ref)) {
        return(sigs)
    }

    # original n of sigs
    ngr0 = length(sigs)
    # original n of genes in sigs
    nge0 = lengths(sigs)
    
    # filter sigs according to ref
    sigs = sapply(sigs, function(group) group[group %in% ref], simplify = F)
    # new n of genes in sigs
    nge1 = lengths(sigs)

    # return NULL if NO genes left
    if (all(nge1 == 0)) {
        return(NULL)
    }
    
    # fractions of genes retained after filtering
    frac.conserved = nge1/nge0

    # filter sigs based on fractions
    sigs = sigs[frac.conserved >= conserved]

    # pretty fractions for printing
    frac.conserved = round(frac.conserved, 2)
    frac.conserved = paste(names(frac.conserved), frac.conserved, sep = ": ")

    # new n of sigs
    # after fraction filtering
    ngr1 = length(sigs)
    
    # stop if no sigs left
    if (ngr1 == 0) {
        stop('No sigs left to score with.')
    }

    # remove filtered-out sigs from n gene counts
    # so that nge0 and nge1 have same (relevant) sigs
    nge1 = nge1[names(ngr1)]
    nge0 = nge0[names(ngr1)]

    # warning if entire sigs were filtered out
    if (ngr1 < ngr0) {
        warning('Removed ', ngr0 - ngr1, ' out of ', ngr0,
                ' sigs with < ', conserved * 100,
                '% genes retained after filtering...')
    }

    # warning if some genes were filtered out from one or more sigs
    if (any(nge1 != nge0)) {
        warning('Some genes were filtered out. Fractions retained:','\n',
                paste0(frac.conserved, collapse = "\n"))
    }

    # return filtered sigs
    sigs
}

#' @title Basic Scoring of Matrix by Gene sigs 
#' @description Average expression level of each column in m for (each) sig(s) in <sigs>.
#' @param m a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.
#' @param sigs a character vector of genes or list of character vectors. Sigs will be filtered to remove genes that are missing from rows in <m>.
#' @param conserved.genes minimum fraction of genes retained in sigs after filtering that is allowed. sigs not passing this cutoff will not be used to score the matrix. This can be ignored if all genes in sigs are present in rownames(<m>). Default: 0.7
#' @return a dataframe of scores, with as many rows as there are columns in <m> and as many columns as there are <sigs> to score against.
#' @rdname baseScores
#' @export 
baseScores = function(m, sigs, conserved.genes = 0.7) {
    if (is.character(sigs)) sigs = list(sigs)
    sigs = filter_sigs(sigs, ref = rownames(m), conserved = conserved.genes)
    sapply(sigs, function(sig) colMeans(m[sig, , drop = F]))
}


#' @title Score a Matrix by Gene sigs (Signatures) 
#' @description Score a Matrix by Gene sigs (Signatures). Raw scores are generated with scalop::baseScores (i.e. average expression level of genes in the signature in question). Options to normalise scores either by centering by the average expression level across all genes for each column of <m> (cells/samples) or by subtracting a score from an expression-bin-matched group. The default is the latter. 
#' @param m a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.
#' @param sigs a character vector of genes or list of character vectors. Sigs will be filtered to remove genes that are missing from rows in <m>.
#' @param groups if scores should be calculated intra-tumour, a list of cell IDs by sample. Default: NULL
#' @param conserved.genes minimum fraction of genes retained in sigs after filtering that is allowed. sigs not passing this cutoff will not be used to score the matrix. This can be ignored if all genes in sigs are present in rownames(<m>). Default: 0.7
#' @param expr.center normalise scores by subtracting scores generated from expression-bin-matched sigs. Set to FALSE if not desired. If expr.center = T and center = T, expr.center takes precedence. Default: T
#' @param center Only relevant if expr.center = FALSE. If TRUE, normalise the scores by subtracting  the mean expression level across all genes (i.e. centering). No scaling is performed. Set to FALSE i f not desired. If expr.center = T and center = T, expr.center takes precedence. Default: T
#' @param expr.bin.m if provided, this matrix will be used to bin the genes and generated expression-bin-matched sigs. Only relevant if expr.center = T. If NULL, <m> will be used. Default: NULL
#' @param expr.bins if provided, these bins will be used to generated bin-matched sigs. Useful if you would like to use the same bins across several scoring analyses, or if your bins control for something other than global gene expression levels. Should be a vector of bin IDs with gene names as vector names. If NULL, will be computed from expr.bin.m (see above). Only relevant if expr.center = T. Default: NULL
#' @param expr.sigs if provided, a character vector or list of character vectors of the same length as <sigs>. Scores with expr.sigs will be subtracted from the real group scores. If NULL and expr.center = T, these will be defined internally as expression-bin-matched sigs to correct the scores. Default: NULL
#' @param expr.nbin if expr.center = T and the expr.sigs are to be generated from scratch, the number of desired expression bins. For example, you should make expr.nbin smaller if you have fewer genes, as you would like a good number of bin-matched genes in each bin group. Default: 30
#' @param expr.binsize the number of bin-matched genes to take per gene in each group. Default: 100
#' @param replace if TRUE, the same bin-matched gene can be taken more than once for the control expr.group. Default: F
#' @return a dataframe of cell/sample scores. Has the same number of rows as there are columns in <m> and the same number of columns as there are sigs to score (assumming no sigs are filtered out because of missing genes).
#' @rdname sigScores
#' @export 
sigScores = function(m,
                     sigs,
                     groups = NULL,
                     center.rows = TRUE,
                     center = T,
                     expr.center = T,
                     expr.bin.m = NULL,
                     expr.bins = NULL,
                     expr.sigs = NULL,
                     expr.nbin = 30,
                     expr.binsize = 100,
                     conserved.genes = 0.7,
                     replace = F) {

    # if only one group, convert to list
    if (is.character(sigs)) sigs = list(sigs)

    # filter sigs
    sigs = filter_sigs(sigs, ref = rownames(m), conserved = conserved.genes)

    Args = mget(ls())

    if (!is.null(groups)) {
        Args$expr.bin.m <- m
        Args = Args[names(Args) != 'm']
        mlist = sapply(groups, function(sample) m[, sample], simplify = F)
        res = sapply(mlist, function(m) {
                         do.call(.sigScores, c(list(m = m), Args))},
                         simplify = F)

        cells = unlist(sapply(res, rownames, simplify = F))
        res = do.call(rbind.data.frame, res)
        rownames(res) = cells
    }

    else res = do.call(.sigScores, Args)

    res
}


.sigScores = function(m,
                      sigs,
                      groups = NULL,
                      center.rows = TRUE,
                      center = T,
                      expr.center = T,
                      expr.bin.m = NULL,
                      expr.bins = NULL,
                      expr.sigs = NULL,
                      expr.nbin = 30,
                      expr.binsize = 100,
                      conserved.genes = 0.7,
                      replace = F) {

    # base scores (no centering / expression normalisation of scores (yet))
    if (center.rows) {
        scores = baseScores(m = rowcenter(m), sigs = sigs, conserved.genes = conserved.genes)
    } else {
        scores = baseScores(m = m, sigs = sigs, conserved.genes = conserved.genes)
    }
    # no mean centering OR expr/complexity centering
    if (!center) expr.center = F

    if (expr.center) {
        if (is.null(expr.sigs)) {

            if (is.null(expr.bins)) {
                if (is.null(expr.bin.m)) expr.bin.m = m
                expr.bins = bin(expr.bin.m, breaks = expr.nbin)
                stopifnot(all(unlist(sigs) %in% names(expr.bins)))
            }

            expr.sigs = sapply(sigs,
                               binmatch,
                               bins = expr.bins,
                               n = expr.binsize,
                               replace = replace,
                               simplify = F)

            names(expr.sigs) = names(sigs)
        }

        if (center.rows) {
            expr.scores = baseScores(m = rowcenter(m), sigs = expr.sigs)
        } else {
            expr.scores = baseScores(m = m, sigs = expr.sigs)
        }
        scores = scores - expr.scores
    }

    else if (center) {
        if (!is.null(expr.bin.m)) center.scores = colMeans(expr.bin.m)
        else center.scores = colMeans(m)
        scores2 = sweep(scores, MARGIN = 1, STATS = center.scores, FUN = "-")
    }

    rows = rownames(scores)
    scores = as.data.frame(scores)
    rownames(scores) = rows
    scores
}

#' @title Score a Matrix with Marker Gene Sets Of Normal Cell Types 
#' @description Score a Matrix with Marker Gene Sets Of Normal Cell Types. Wrapper around scalop::sigScores. Please see `?scalop::sigScores` for more details
#' @param m a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.
#' @param ... other arguments passed to scalop::sigScores, although none are required. 
#' @details the gene sets can be seen in Markers_Normal
#' @return a dataframe of cell/sample scores. Each column describes scores for a different Marker Gene Set.  
#' @rdname markerScores
#' @export 
markerScores = function(m, ...) {
    sigs = suppressWarnings(filter_sigs(Markers_Normal, ref = rownames(m), conserved = 0.4))
    sigs = sigs[lengths(sigs) >= 2]
    sigScores(m = m, sigs = sigs, conserved.genes = conserved, ...)
} 
