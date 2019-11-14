
.clusterSizeCutoff = function(cluster.size, ncells) {
    if (cluster.size >= 0 & cluster.size <= 1) {
	    cluster.size = cluster.size * ncells
    }

    cluster.size
}

.clusterNames = function(cutreeOutput) {
    # change df colnames (tree heights) to have 4 decimal places followed by "_"
    colnames(cutreeOutput) = paste0(round(as.numeric(colnames(cutreeOutput)), 4), "_")
    # new clusterNames
    names(unlist(apply(cutreeOutput, 2, function(col) 1:length(table(col)))))
}

.extractClusters = function(hc = NULL,
                     	    k = NULL,
                            h = NULL,
                            min.cluster.size = 0,
                            max.cluster.size = 1) {

    if (is.null(h) & is.null(k)) h = hc$height # all heights
    Clusters = stats::cutree(tree = hc, h = h, k = k)
    clusterNames = .clusterNames(cutreeOutput = Clusters)
    labels = rownames(Clusters)
    Clusters = as.list(as.data.frame(Clusters))
    Clusters = sapply(Clusters, function(ID) split(labels, ID), simplify =F)
    Clusters = unlist(Clusters, recursive = F, use.names = F)
    Clusters = stats::setNames(Clusters, clusterNames)
    ncells = length(unique(unlist(Clusters)))
    min.cluster.size = .clusterSizeCutoff(min.cluster.size, ncells)
    max.cluster.size = .clusterSizeCutoff(max.cluster.size, ncells)
    lens = lengths(Clusters)
    Clusters = Clusters[lens >= min.cluster.size & lens <= max.cluster.size]
    Clusters
}

