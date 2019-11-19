.hca_cor = function(m, method = cor.methods) {
    method = match.arg(method)
    stats::cor(m, method = method)
}

.hca_dist = function(m, maxd = 1, method = dist.methods) {
    if (is.null(maxd)) dm = m
    else {
        if (is_square(m) && unique(diag(m)) != maxd) {
            warning("max. distance <maxd> = ", maxd,
                    " but <m> diagonal = ", unique(diag(m)), "...")
        }
        dm = maxd - m
    }
    if (is.null(method)) return(stats::as.dist(dm))
    method = match.arg(method)
    stats::dist(dm, method = method)
}

.hca_tree = function(d, method = cluster.methods) {
    method = match.arg(method)
    stats::hclust(d, method = method)
}

.hca_cutree = function(tree, k, h, groups.minsize, groups.maxsize) {
    groups = .hca_cutree_as_list(tree = tree, k = k, h = h)
    ncells = length(tree$labels)
    groups.minsize = .cluster_size(groups.minsize, ncells)
    groups.maxsize = .cluster_size(groups.maxsize, ncells)
    lens = lengths(groups)
    groups[lens >= groups.minsize & lens <= groups.maxsize]
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
                h = NULL,
                k = NULL,
                groups.minsize = 5,
                groups.maxsize = 0.5, 
                cor.end = F,
                dist.end = F,
                hclust.end = F) {

    res = c()

    if (class(x) == 'matrix') {
        if (!is.null(cor.method)) {
            if (is_cor(x)) {
                warning('\nComputing correlations over a correlation matrix...\n',
                        'Set `cor.method = NULL` to skip correlation step.')} 
            x = .hca_cor(x, method = cor.method)
            res = c(res, list(cr = x))
            if (cor.end) return(res)}
        x = .hca_dist(x, method = dist.method)
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
                        groups.minsize = groups.minsize, 
                        groups.maxsize = groups.maxsize)
        c(res, list(groups = x))
    }

    res
}

