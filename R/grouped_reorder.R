
#' @title Grouped Reorder and Ungroup 
#' @description Keep desired groups together but reorder members within each group. E.g. If cells from multiple samples are being reordered, samples can be kept together and cells reordered within. Option to also reorder the order of groups or samples, such that groups with similar average profiles are placed next to one another. 
#' @param m matrix to be reordered
#' @param groups groups within which reordering should take place.
#' @param interorder if TRUE, group order itself is reordered such that groups with similar profiles are placed near one another. E.g. Samples with similar average CNA profiles. Default: FALSE
#' @param cor.method desired correlation metric. Default: 'pearson'
#' @param dist.method desired distance metric to be used on top of correlation matrix. Default: 'euclidean'
#' @param cluster.method desired agglomeration method. Default: 'average'
#' @param Names return the vector of ordered IDs instead of the reordered matrix. Default: FALSE
#' @return reordered matrix (same dimensions as input) or a character vector of ordered column names if Names = T. 
#' @rdname grouped_reorder
#' @export 
grouped_reorder = function(m,
                           groups,
                           interorder = FALSE,
                           intraorder = TRUE,
                           cor.method = 'pearson',
                           dist.method = 'euclidean',
                           cluster.method = 'average',
                           Names = FALSE) {

    if (interorder) {
        groupAvgs = sapply(groups, function(group) rowMeans(m[, group, drop = F]))
        groups = groups[hca_order(rowcenter(groupAvgs))]
    }

    if (intraorder) {
        m.list = sapply(groups, function(x) m[, x], simplify = F)
        groups = sapply(m.list, function(m) hca_order(rowcenter(m),
                                                      cor.method = cor.method,
                                                      dist.method = dist.method,
                                                      cluster.method = cluster.method))
    }

    if (Names) {
        return(groups)
    }

    ord = as.character(unlist(groups, use.names = F))
    m[, ord]
}
