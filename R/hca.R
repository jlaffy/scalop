#' @title Hierarchical clustering analysis
#' @description hca takes an expression matrix as input and works through sequentially-dependent computations to produce a list with the following objects:
#' 1. 'm': your input (expression) matrix
#' 2. 'cr': a correlation (or similarity) matrix 
#' 3. 'dst': a distance matrix
#' 4. 'hc': a hierarchical clustering object
#'      + 'ord': the clustering order (only useful as output)
#' 5. 'clusters': a list of cluster groups 
#' @md
#' @param m input matrix or NULL. If NULL, an object must be provided to one of 'cr', 'dst' or 'hc'. Default: NULL
#' @param cr,dst,hc,ord,clusters the relevant hca object (specifications below) or logical. If an object, hca() starts with this as input. Later inputs override earlier ones, except 'ord' and 'clusters' which are ignored. If TRUE, hca() stops after the object is generated. If FALSE, then the object is computed and the function proceeds, other arguments permitting. Default: FALSE 
#' @param cr correlation or similarity matrix or logical. Default: FALSE
#' @param dst distance matrix of class 'dist' or logical. Default: FALSE
#' @param hc hierarchical clustering object of class 'hclust' or logical. Default: FALSE
#' @param ord logical. An order vector will be ignored. Default: FALSE
#' @param clusters logical. Default: FALSE
#' @param return.steps logical indicating whether to return intermediary steps when a subset of the arguments are computed.. Default: FALSE
#' @param hc.method linkage method. Default: 'average'
#' @param cor.method correlation coefficient. Default: 'pearson'
#' @param compute.dist logical. If FALSE, 'cr' is coerced to a distance matrix. If TRUE, distances are calculated from 'cr'. Default: T
#' @param dist.method string specifying distance metric; ignored if compute.dist = F. Default: 'euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @param ... see arguments in hca for details. 
#' @return object from hca call.
#' @details It is up to you to provide the correct argument(s) to the functions:  
#' @details hca_cr must take <m>  
#' @details hca_dst must take <m> or <cr>  
#' @details hca_hc can take <m>, <cr> or <dst>  
#' @details hca_ord can take <m>, <cr>, <dst> or <hc>  
#' @details hca_clusters can take any of the above.
#' @details By default, hca returns a list containing all above mentioned objects.
#' @details To break from the function after your object of interest has been computed, set the corresponding argument to TRUE. Note that when an argument is set to TRUE, only that argument is returned unless return.steps = T. 
#' @details To begin the function from a precomputed object, pass to the appropriate argument. This allows you to skip precomputed steps and provide custom objects -- for example a similarity matrix (instead of the default correlation matrix computed in 'cr').
#' @details The hca_[*] wrapper functions act as a shorthand to retrieve specific objects (replace [*] with object name). hca_* wrappers have simpler syntax but always return _one_ object.
#' @return object or list of objects. If the latter, a full list contains m (input matrix to be clustered), cr (correlation matrix), dst (distance matrix), hc (hclust object), ord (char. vector), clusters (list of char. vectors).
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @examples hca_clusters(hc = hc)
#' @examples hca_ord(m = m)
#' @rdname hca
#' @export 
#' @importFrom stats cor hclust dist as.dist
hca = function(m = NULL,
               cr = FALSE,
               dst = FALSE,
               hc = FALSE,
               ord = FALSE,
               clusters = F,
               return.steps = TRUE,
               hc.method = 'average', 
               cor.method = 'pearson',
               compute.dist = T,
               dist.method = 'euclidean',
               ord.labels = T,
  		       h = NULL,
		       k = NULL,
		       min.cluster.size = 5,
		       max.cluster.size = 0.8) {

  # CORRELATION MATRIX
  # run?
    
    List = c()
    objects_to_compute = list(cr, dst, hc, ord, clusters)
    start_computation = 0
    end_computation = 6
    custom_start = sapply(objects_to_compute, function(obj) !is.logical(obj))
    custom_end = sapply(objects_to_compute, isTRUE)

    if (any(custom_start)) {
        start_computation = max(which(custom_start))
    }

    if (any(custom_end)) {
        end_computation = max(which(custom_end))
    }

    if (start_computation == 0) {
        List = c(List, list(m = m))
        cr = stats::cor(m, method = cor.method)
        cr[is.na(cr)] <- 0
        start_computation = start_computation + 1
    }

    List = c(List, list(cr = cr))
    
    if (end_computation == 1) {
        if (return.steps) return(List)
        return(cr)
    }

    if (start_computation == 1) {
        if (compute.dist) dst = stats::dist(1 - cr, method = dist.method)
        else dst = stats::as.dist(1 - cr)
        start_computation = start_computation + 1
    }

    List = c(List, list(dst = dst))
    
    if (end_computation == 2) {
        if (return.steps) return(List)
        return(dst)
    }

    if (start_computation == 2) {
        hc = stats::hclust(dst, method = hc.method)
        start_computation = start_computation + 1
    }

    List = c(List, list(hc = hc))

    if (end_computation == 3) {
        if (return.steps) return(List)
        else return(hc)
    }

    if (start_computation == 3) {
        if (!ord.labels) ord = hc$order
        else ord = hc$labels[hc$order]
        start_computation = start_computation + 1
    }

    List = c(List, list(ord = ord))

    if (end_computation == 4) {
        if (return.steps) return(List)
        else return(ord)
    }

    if (start_computation == 4) {
	    clusters = .extractClusters(hc = hc,
				                    h = h,
				                    k = k,
				                    min.cluster.size = min.cluster.size,
				                    max.cluster.size = max.cluster.size)
        start_computation = start_computation + 1
    }

    List = c(List, list(clusters = clusters))

    if (end_computation == 5) {
        if (return.steps) return(List)
        else return(clusters)
    }

    List
}


 
#' @rdname hca
#' @export 
hca_cr = function(m, ...) {
    hca(m = m, cr = T, ...)$cr
}

#' @rdname hca
#' @export 
hca_dst = function(...) {
    hca(dst = T, ...)$dst
}

#' @rdname hca
#' @export 
hca_hc = function(...) {
    hca(hc = T, ...)$hc
}


#' @rdname hca
#' @export 
hca_ord = function(...) {
    hca(ord = T, ...)$ord
}

#' @rdname hca
#' @export 
hca_clusters = function(...) {
    hca(...)$clusters
}

hca_reord = function(m, row = T, col = T, ...) {
    if (col) m = m[, hca_ord(m = m, ...)]
    if (row) m = m[hca_ord(m = t(m), ...), ]
    m
}

hca_creord = function(m, row = T, col = T, ...) {
    c(cr, ord) %<-% hca(m = m, ord = T, ...)[c("cr", "ord")]
    if (col) cr = cr[, ord]
    if (row) cr = cr[ord, ]
    cr
}
