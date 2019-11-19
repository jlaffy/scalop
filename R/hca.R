#' @title Hierarchical clustering analysis
#' @description Hierarchical clustering analysis, beginning from a matrix, a `dist` object or a `hclust` object. All subsequent objects necessary to be obtained groups or clusters from the clustering analysis will be generated. Use hca_[*] where * an object in question to stop the ste
#' @param x a matrix (features X observations), a dist object or hclust object.
#' @param cor.method if correlation is desired, a character string, else NULL. If character string, one of 'pearson' (default), 'kendall' or 'spearman'. Applicable only when <x> is a matrix. One of scalop::cor.methods.  If correlation of <x> is not desired (e.g. if <x> is already a correlation or similarity mabtrix), set cor.method = NULL. Else 'pearson'. If <x> is a correlation/similarity matrix, set cor.method = NULL. Default: cor.methods
#' @param dist.method NULL or a character string, where default is 'euclidean'. See scalop::dist.methods for alternatives. Default: dist.methods
#' @param cluster.method character string; one of scalop::cluster.methods. Default is 'average'. Default: cluster.methods
#' @param h height(s) at which to cut the tree. If NULL, h will be set to all tree heights. Default: NULL
#' @param k number of groups to return from tree. If <h> and <k> are both not NULL, <k> takes precedence. Default: NULL
#' @param groups.minsize PARAM_DESCRIPTION, Default: 5
#' @param groups.maxsize PARAM_DESCRIPTION, Default: 0.5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname hca
#' @export 
hca = function(x,
               cor.method = cor.methods,
               dist.method = dist.methods,
               cluster.method = cluster.methods,
               h = NULL,
               k = NULL,
               groups.minsize = 5,
               groups.maxsize = 0.5) {
    .hca(x,
         cor.method = cor.methods,
         dist.method = dist.methods,
         cluster.method = cluster.methods,
         h = h,
         k = k,
         groups.minsize = groups.minsize,
         groups.maxsize = groups.maxsize) 
}


#' @rdname hca
#' @export 
hca_cor = function(x, return.steps = F, reorder = T, reorder.col = reorder, reorder.row = reorder, ...) {
    if (reorder.col|reorder.row) {
        obj = .hca(x, hclust.end = T, ...)
        if (reorder.col) obj$cr = obj$cr[, obj$ord]
        if (reorder.row) obj$cr = obj$cr[obj$ord, ...]
    } else obj = .hca(x, cor.end = T, ...)
    if (return.steps) return(obj)
    obj$cr
}

#' @rdname hca
#' @export 
hca_dist = function(x, return.steps = F, ...) {
    if (return.steps) return(.hca(x, dist.end = T, ...))
    .hca(x, dist.end = T, ...)$dist
}

#' @rdname hca
#' @export 
hca_tree = function(x, return.steps = F, ...) {
    if (return.steps) return(.hca(x, hclust.end = T, ...))
    .hca(x, hclust.end = T, ...)$tree
}

#' @rdname hca
#' @export 
hca_order = function(x, return.steps = F, ...) {
    if (return.steps) return(.hca(x, hclust.end = T, ...))
    .hca(x, hclust.end = T, ...)$order
}

#' @rdname hca
#' @export 
hca_groups = function(x, return.steps = F, ...) {
    if (return.steps) return(.hca(x, ...))
    .hca(x, ...)$groups
}


#' @rdname hca
#' @export 
hca_reorder = function(x, col = T, row = T, cor.force = F, ...) {
    stopifnot(has_dim(x))
    skip.cor = (!isTRUE(cor.force) & is_cor(x))
    if (skip.cor) {
        cor.method = NULL
        message('Input matrix <x> is a correlation matrix. Skipping correlation...')
        message('Set cor.force = T to force correlation step.')
        ord = .hca(x = x, hclust.end = T, cor.method = NULL, ...)$order
        if (col) x = x[, ord]
        if (row) x = x[ord, ]
    } else {
        if (col) x = x[, .hca(x = x, hclust.end = T, ...)$order]
        if (row) x = x[.hca(x = t(x), hclust.end = T, ...)$order, ]
    } 
    x
}
