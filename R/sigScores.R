
#' @title Filter genes in groups according to reference 
#' @description Filter genes in groups according to reference 
#' @param groups a character vector of genes or list of character vectors to filter 
#' @param ref reference genes to filter groups according to
#' @param conserved the minimum allowed fraction of genes retained after filtering. Default: 0.7
#' @return a filtered list of groups. Returns NULL if no genes in any groups are left. Groups with less than <conserved> fraction of genes retained will be removed. Returns input <groups> if no genes are missing.
#' @rdname filter_groups
#' @export 
filter_groups = function(groups, ref, conserved = 0.7) {
    # no need for filtering
    if (all(unlist(groups) %in% ref)) {
        return(groups)
    }

    # original n of groups
    ngr0 = length(groups)
    # original n of genes in groups
    nge0 = lengths(groups)
    
    # filter groups according to ref
    groups = sapply(groups, function(group) group[group %in% ref], simplify = F)
    # new n of genes in groups
    nge1 = lengths(groups)

    # return NULL if NO genes left
    if (all(nge1 == 0)) {
        return(NULL)
    }
    
    # fractions of genes retained after filtering
    frac.conserved = nge1/nge0

    # filter groups based on fractions
    groups = groups[frac.conserved >= conserved]

    # pretty fractions for printing
    frac.conserved = round(frac.conserved, 2)
    frac.conserved = paste(names(frac.conserved), frac.conserved, sep = ": ")

    # new n of groups
    # after fraction filtering
    ngr1 = length(groups)
    
    # stop if no groups left
    if (ngr1 == 0) {
        stop('No groups left to score with.')
    }

    # remove filtered-out groups from n gene counts
    # so that nge0 and nge1 have same (relevant) groups
    nge1 = nge1[names(ngr1)]
    nge0 = nge0[names(ngr1)]

    # warning if entire groups were filtered out
    if (ngr1 < ngr0) {
        warning('Removed ', ngr0 - ngr1, ' out of ', ngr0,
                ' groups with < ', conserved * 100,
                '% genes retained after filtering...')
    }

    # warning if some genes were filtered out from one or more groups
    if (any(nge1 != nge0)) {
        warning('Some genes were filtered out. Fractions retained:','\n',
                paste0(frac.conserved, collapse = "\n"))
    }

    # return filtered groups
    groups
}

#' @title Basic Scoring of Matrix by Gene Groups 
#' @description Average expression level of each column in m for (each) group(s) in <groups>.
#' @param m a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.
#' @param groups a character vector of genes or list of character vectors. Groups will be filtered to remove genes that are missing from rows in <m>.
#' @param conserved.genes minimum fraction of genes retained in groups after filtering that is allowed. groups not passing this cutoff will not be used to score the matrix. This can be ignored if all genes in groups are present in rownames(<m>). Default: 0.7
#' @return a dataframe of scores, with as many rows as there are columns in <m> and as many columns as there are <groups> to score against.
#' @rdname baseScores
#' @export 
baseScores = function(m, groups, conserved.genes = 0.7) {
    if (is.character(groups)) groups = list(groups)
    groups = filter_groups(groups, ref = rownames(m), conserved = conserved.genes)
    sapply(groups, function(group) colMeans(m[group, , drop = F]))
}


#' @title Score a Matrix by Gene Groups (Signatures) 
#' @description Score a Matrix by Gene Groups (Signatures). Raw scores are generated with scalop::baseScores (i.e. average xpression level of genes in the group in question). Options to normalise scores either by centering by the average expression level across all genes for each column of <m> (cells/samples) or by subtracting a score from an expression-bin-matched group. The default is the latter. 
#' @param m a non-centered matrix of genes X cells/samples. The matrix will be row-centered internally prior to scoring.
#' @param groups a character vector of genes or list of character vectors. Groups will be filtered to remove genes that are missing from rows in <m>.
#' @param conserved.genes minimum fraction of genes retained in groups after filtering that is allowed. groups not passing this cutoff will not be used to score the matrix. This can be ignored if all genes in groups are present in rownames(<m>). Default: 0.7
#' @param expr.center normalise scores by subtracting scores generated from expression-bin-matched groups. Set to FALSE if not desired. If expr.center = T and center = T, expr.center takes precedence. Default: T
#' @param center Only relevant if expr.center = FALSE. If TRUE, normalise the scores by subtracting  the mean expression level across all genes (i.e. centering). No scaling is performed. Set to FALSE i f not desired. If expr.center = T and center = T, expr.center takes precedence. Default: T
#' @param expr.bin.m if provided, this matrix will be used to bin the genes and generated expression-bin-matched groups. Only relevant if expr.center = T. If NULL, <m> will be used. Default: NULL
#' @param expr.bins if provided, these bins will be used to generated bin-matched groups. Useful if you would like to use the same bins across several scoring analyses, or if your bins control for something other than global gene expression levels. Should be a vector of bin IDs with gene names as vector names. If NULL, will be computed from expr.bin.m (see above). Only relevant if expr.center = T. Default: NULL
#' @param expr.groups if provided, a character vector or list of character vectors of the same length as <groups>. Scores with expr.groups will be subtracted from the real group scores. If NULL and expr.center = T, these will be defined internally as expression-bin-matched groups to correct the scores. Default: NULL
#' @param expr.nbin if expr.center = T and the expr.groups are to be generated from scratch, the number of desired expression bins. For example, you should make expr.nbin smaller if you have fewer genes, as you would like a good number of bin-matched genes in each bin group. Default: 30
#' @param expr.binsize the number of bin-matched genes to take per gene in each group. Default: 100
#' @param replace if TRUE, the same bin-matched gene can be taken more than once for the control expr.group. Default: F
#' @return a dataframe of cell/sample scores. Has the same number of rows as there are columns in <m> and the same number of columns as there are groups to score (assumming no groups are filtered out because of missing genes).
#' @rdname sigScores
#' @export 
sigScores = function(m,
                     groups,
                     center = T,
                     expr.center = T,
                     expr.bin.m = NULL,
                     expr.bins = NULL,
                     expr.groups = NULL,
                     expr.nbin = 30,
                     expr.binsize = 100,
                     conserved.genes = 0.7,
                     replace = F) {

    # if only one group, convert to list
    if (is.character(groups)) groups = list(groups)
    
    # filter groups
    groups = filter_groups(groups, ref = rownames(m), conserved = conserved.genes)

    # base scores (no centering / expression normalisation of scores (yet))
    scores = baseScores(m = rowcenter(m), groups = groups, conserved = conserved.genes)

    # no mean centering OR expr/complexity centering
    if (!center) expr.center = F

    if (expr.center) {
        if (is.null(expr.groups)) {

            if (is.null(expr.bins)) {
                if (is.null(expr.bin.m)) expr.bin.m = m
                expr.bins = bin(expr.bin.m, breaks = expr.nbin)
                stopifnot(all(unlist(groups) %in% names(expr.bins)))
            }

            expr.groups = sapply(groups,
                                 binmatch,
                                 bins = expr.bins,
                                 n = expr.binsize,
                                 replace = replace,
                                 simplify = F)

            names(expr.groups) = names(groups)
        }

        expr.scores = baseScores(m = rowcenter(expr.bin.m), groups = expr.groups)
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
    groups = suppressWarnings(filter_groups(Markers_Normal, ref = rownames(m), conserved = 0.4))
    groups = groups[lengths(groups) >= 2]
    sigScores(m = m, groups = groups, conserved.genes = conserved, ...)
} 
