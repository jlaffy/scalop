
grouped_order = function(m,
                         groups,
                         cor.method = 'pearson',
                         dist.method = 'euclidean',
                         cluster.method = 'average',
                         Names = FALSE) {

    m.list = sapply(groups, function(x) m[, x], simplify = F)
    ord.list = sapply(m.list, function(m) hca_order(m,
                                                    cor.method = cor.method,
                                                    dist.method = dist.method,
                                                    cluster.method = cluster.method))

    ord = as.character(unlist(ord.list, use.names = F))
    if (Names) return(ord)
    m[, ord]
}
