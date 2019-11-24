#' @title Hierarchical clustering analysis
#' @description Hierarchical clustering analysis
#' @param x a matrix (features X observations), an object of class `dist` or an object of class `hclust`.
#' @param cor.method option if <x> is a matrix. One of 'pearson' (default), 'kendall', 'spearman' or NULL (no correlation coefficents computed). Default: scalop::cor.methods
#' @param dist.method option if <x> is a matrix or correlation matrix. One of 'euclidean' (default), 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski' or NULL (in which case stats::as.dist() will be used). Default: scalop::dist.methods
#' @param max.dist maximum distance between observations. Default: 1 
#' @param cluster.method one of "average" (default), "complete", "single", "ward.D", "ward.D2", "mcquitty", "median" or "centroid". Default: scalop::cluster.methods
#' @param h height(s) at which to cut the tree. If NULL, h will be set to all tree heights. Default: NULL
#' @param k number of groups to return from tree. If <h> and <k> are both not NULL, <k> takes precedence. Default: NULL
#' @param min.size PARAM_DESCRIPTION, Default: 5
#' @param max.size PARAM_DESCRIPTION, Default: 0.5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname hca
#' @export 
hca = function(x,
               cor.method = cor.methods,
               dist.method = dist.methods,
               cluster.method = cluster.methods,
               max.dist = 1,
               h = NULL,
               k = NULL,
               min.size = 5,
               max.size = 0.5) {
    .hca(x,
         cor.method = cor.method,
         dist.method = dist.method,
         cluster.method = cluster.method,
         max.dist = max.dist,
         h = h,
         k = k,
         min.size = min.size,
         max.size = max.size) 
}


#' @rdname hca
#' @export 
hca_cor = function(x, return.steps = F, reorder = T, reorder.col = reorder, reorder.row = reorder, ...) {
    if (reorder.col|reorder.row) {
        obj = .hca(x, hclust.end = T, ...)
        if (reorder.col) obj$cr = obj$cr[, obj$ord]
        if (reorder.row) obj$cr = obj$cr[obj$ord, ]
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
        message('\nInput matrix <x> is a correlation matrix. Skipping correlation...',
                '\nSet cor.force = T to force correlation step.')
        ord = .hca(x = x, hclust.end = T, cor.method = NULL, ...)$order
        if (col) x = x[, ord]
        if (row) x = x[ord, ]
    } else {
        if (col) x = x[, .hca(x = x, hclust.end = T, ...)$order]
        if (row) x = x[.hca(x = t(x), hclust.end = T, ...)$order, ]
    } 
    x
}
