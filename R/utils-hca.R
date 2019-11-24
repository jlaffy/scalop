.hca_cor = function(m, method) {
    stats::cor(m, method = method)
}

.hca_dist = function(m, method, max.dist = NULL) {
    if (is.null(max.dist)) dm = m
    else {
        if (is_square(m) && unique(diag(m)) != max.dist) {
            warning("max. distance <max.dist> = ", max.dist,
                    " but <m> diagonal = ", unique(diag(m)), "...")
        }
        dm = max.dist - m
    }
    if (is.null(method)) return(stats::as.dist(dm))
    stats::dist(dm, method = method)
}

.hca_tree = function(d, method) {
    stats::hclust(d, method = method)
}

.hca_cutree = function(tree, k, h, min.size, max.size) {
    groups = .hca_cutree_as_list(tree = tree, k = k, h = h)
    ncells = length(tree$labels)
    min.size = .cluster_size(min.size, ncells)
    max.size = .cluster_size(max.size, ncells)
    lens = lengths(groups)
    groups[lens >= min.size & lens <= max.size]
}

.cluster_size = function(cluster.size, ncells) {
    stopifnot(cluster.size >= 0)
    stopifnot(cluster.size <= ncells)
    if (cluster.size > 1) return(cluster.size)
    cluster.size * ncells
}

.hca_cutree_as_list = function(tree, h, k) {
    stopifnot(!is.null(h) | !is.null(k))
    groups = stats::cutree(tree = tree, h = h, k = k)
    if (!has_dim(groups)) return(split(names(groups), groups))
    
    .clusterNames = function(cutreeOutput) {
        # change df colnames (tree heights) to have 4 decimal places followed by "_"
        colnames(cutreeOutput) = paste0(round(as.numeric(colnames(cutreeOutput)), 4), "_")
        # new clusterNames
        names(unlist(apply(cutreeOutput, 2, function(col) 1:length(table(col)))))
    }

    clusterNames = .clusterNames(cutreeOutput = groups)
    labels = rownames(groups)
    groups = as.list(as.data.frame(groups))
    groups = sapply(groups, function(ID) split(labels, ID), simplify =F)
    groups = unlist(groups, recursive = F, use.names = F)
    stats::setNames(groups, clusterNames)
}

.hca = function(x,
                cor.method = cor.methods, 
                dist.method = dist.methods,
                cluster.method = cluster.methods,
                max.dist = 1,
                h = NULL,
                k = NULL,
                min.size = 5,
                max.size = 0.5, 
                cor.end = F,
                dist.end = F,
                hclust.end = F) {

    res = c()
    if (!is.null(cor.method)) cor.method = match.arg(cor.method)
    if (!is.null(dist.method)) dist.method = match.arg(dist.method)
    cluster.method = match.arg(cluster.method)

    if (class(x) == 'matrix') {
        if (!is.null(cor.method)) {
            if (is_cor(x)) {
                warning('\nComputing correlations over a correlation matrix...\n',
                        'Set `cor.method = NULL` to skip correlation step.')
                x = .hca_cor(x, method = cor.method)
            }
            res = c(res, list(cr = x))
            if (cor.end) return(res)
        }
        x = .hca_dist(x, method = dist.method, max.dist = max.dist)
        res = c(res, list(dist = x))
        if (dist.end) return(res)
    }

    if (class(x) == 'dist') {
        x = .hca_tree(x, method = cluster.method)
        res = c(res, list(tree = x, order = x$labels[x$order]))
        if (hclust.end) return(res)
    }

    if (class(x) == 'hclust') {
        if (is.null(h)) h = res$tree$height
        x = .hca_cutree(x,
                        k = k,
                        h = h,
                        min.size = min.size, 
                        max.size = max.size)
        res = c(res, list(groups = x))
    }

    res
}

