.hca = function(x, cor.method = scalop::cor.methods, dist.method = scalop::dist.methods, cluster.method = scalop::cluster.methods, max.dist = 1, h = NULL, k = NULL, min.size = 5, max.size = 0.5, cor.end = F, dist.end = F, hclust.end = F) {
    res = c()
    if ('matrix' %in% class(x)) {
        x = .hca_cor(x, method = cor.method)
        res = c(res, list(cr = x))
        if (cor.end)
            return(res)
        x = .hca_dist(x, method = dist.method, max.dist = max.dist)
        res = c(res, list(dist = x))
        if (dist.end)
            return(res)
    }
    if (class(x) == "dist") {
        x = .hca_tree(x, method = cluster.method)
        res = c(res, list(tree = x, order = x$labels[x$order]))
        if (hclust.end)
            return(res)
    }
    if (class(x) == "hclust") {
        if (is.null(h))
            h = res$tree$height
        x = .hca_cutree(x, k = k, h = h, min.size = min.size,
            max.size = max.size)
        res = c(res, list(groups = x))
    }
    res
}


hca_reorder = function(x, col = T, row = T, cor.method = "none", ...){
    if (col)
        x = x[, .hca(x = x, hclust.end = T, cor.method = cor.method,
            ...)$order]
    if (row)
        x = x[.hca(x = t(x), hclust.end = T, cor.method = cor.method,
            ...)$order, ]
    x
}


hca_cor = function(x, return.steps = F, reorder = T, reorder.col = reorder,
    reorder.row = reorder, ...)
{
    if (reorder.col | reorder.row) {
        obj = .hca(x, hclust.end = T, ...)
        if (reorder.col)
            obj$cr = obj$cr[, obj$ord]
        if (reorder.row)
            obj$cr = obj$cr[obj$ord, ]
    }
    else obj = .hca(x, cor.end = T, ...)
    if (return.steps)
        return(obj)
    obj$cr
}


.hca_cor = function(m, method) {
    method = match.arg(method, scalop::cor.methods)
    if (method == "none")
        return(m)
    if (is_cor(m)) {
        warning("\nComputing correlations over a correlation matrix...\n",
            "Set `cor.method = \"none\"` to skip correlation step.")
    }
    stats::cor(m, method = method)
}


.hca_dist = function(m, method, max.dist = NULL) {
    method = match.arg(method, scalop::dist.methods)
    if (!is_square(m))
        m = t(m)
    #if (!is.null(max.dist)) {
    #    if (is_square(m) && unique(diag(m)) != max.dist) {
    #        warning("<max.dist> = ", max.dist, " but <m> diagonal = ",
    #            unique(diag(m)), "...")
    #    }
    #    m = max.dist - m
    #}
    if (method == "none")
        return(stats::as.dist(m))
    stats::dist(m, method = method)
}

.hca_tree = function(d, method)
{
    method = match.arg(method, scalop::cluster.methods)
    stats::hclust(d, method = method)
}

hca_groups = function (x, return.steps = F, ...)
{
    if (return.steps)
        return(.hca(x, ...))
    .hca(x, ...)$groups
}

.hca_cutree = function (tree, k, h, min.size, max.size)
{
    groups = .hca_cutree_as_list(tree = tree, k = k, h = h)
    ncells = length(tree$labels)
    min.size = .cluster_size(min.size, ncells)
    max.size = .cluster_size(max.size, ncells)
    lens = lengths(groups)
    groups[lens >= min.size & lens <= max.size]
}

.hca_cutree_as_list = function (tree, h, k)
{
    stopifnot(!is.null(h) | !is.null(k))
    groups = stats::cutree(tree = tree, h = h, k = k)
    if (!has_dim(groups))
        return(split(names(groups), groups))
    .clusterNames = function(cutreeOutput) {
        colnames(cutreeOutput) = paste0(round(as.numeric(colnames(cutreeOutput)),
            4), "_")
        names(unlist(apply(cutreeOutput, 2, function(col) 1:length(table(col)))))
    }
    clusterNames = .clusterNames(cutreeOutput = groups)
    labels = rownames(groups)
    groups = as.list(as.data.frame(groups))
    groups = sapply(groups, function(ID) split(labels, ID), simplify = F)
    groups = unlist(groups, recursive = F, use.names = F)
    groups = stats::setNames(groups, clusterNames)
    unique(groups)
}

.cluster_size = function (cluster.size, ncells)
{
    stopifnot(cluster.size >= 0)
    stopifnot(cluster.size <= ncells)
    if (cluster.size > 1)
        return(cluster.size)
    cluster.size * ncells
}


programs = function (m, groups = NULL, nsig1 = 50, nsig2 = 10, jaccard = 0.7, p = 0.01, lfc = log2(2), pmethod = "BH") {
    library(scalop)
    if (is.null(groups)) {
        groups = hca_groups(rowcenter(m))
    }
    deas = dea(m, group = groups, return.val = "df", p = p, lfc = lfc,
        arrange.by = "lfc", pmethod = pmethod)
    sig1 = sapply(deas, function(df) sum(df$p.adj <= 0.01, na.rm = TRUE))
    sig2 = sapply(deas, function(df) sum(df$p.adj <= 0.001, na.rm = TRUE))
    sig3 = sapply(deas, function(df) sum(df$p.adj <= 1e-04, na.rm = TRUE))
    bool = sig1 >= nsig1 & sig2 >= nsig2
    deas = deas[bool]
    groups = groups[bool]
    sig1 = sig1[bool]
    sig2 = sig2[bool]
    sig3 = sig3[bool]
    ord = order(sig1, sig2, sig3, decreasing = T)
    deas = deas[ord]
    groups = groups[ord]
    sig1 = sig1[ord]
    sig2 = sig2[ord]
    sig3 = sig3[ord]
    jac.pass = jacFilt(groups, threshold = jaccard, which = TRUE)
    deas = deas[jac.pass]
    groups = groups[jac.pass]
    sig1 = sig1[jac.pass]
    sig2 = sig2[jac.pass]
    sig3 = sig3[jac.pass]
    programs = sapply(deas, function(d) d$gene[1:nsig1], simplify = F)
    lfcs = dea(m, group = groups, return.val = "lfc", arrange.by = "none",
        p = NULL, lfc = NULL, pmethod = pmethod)
    list(programs = programs, profiles = lfcs, groups = groups,
        deas = deas, sig1 = sig1, sig2 = sig2, sig3 = sig3)
}

