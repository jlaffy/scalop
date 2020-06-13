#' @title Identify potential doublet cells 
#' @description Identify potential doublet cells using cluster assignments. Imagine the following scenario: we have a macrophage-endothelial doublet cell; an endothelial cell cluster, and a macrophage cluster in which the doublet cell is currently 'hidden'. We take the residual profile of the hidden doublet cell against its cluster centroid (the average expression profile of same-sample cells in its cluster). Since the cluster is a macrophage cluster, what we are theoretically left with is an endothelial expression profile. We then look for correlations between this cell's residual profile and the residual profile of every other cluster centroid to the same original centroid (that of the macrophage clsuter in which the doublet cell resides). The endothelial cluster centroid - the macrophage cluster centroid will leave us with an endothelial expression profile, which should have high correlation to the residual profile of the doublet cell.
#' @param m matrix of genes by cells expression values  
#' @param clusters list of cell IDs in each cluster  
#' @param samples character vector of unique sample ids. These are used to calculate sample-specific cluster centroids, since doublets can only occur between cells coming from the same sample.
#' @param clusters_to_check the clusters to look for doublet cells in. These are checked against the clusters in <clusters>. Default: clusters
#' @param return.type "matrix" returns the matrix of cluster by cell correlation values, with the cells from each cluster combined into one matrix. "list" returns a list of matrices, with the cells in each cluster in separate matrices. Default: c("matrix", "list")
#' @return a matrix or list of matrices. Each matrix is of the form cluster X cell and is described by their respective pairwise pearson correlation values.
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[stats]{setNames}},\code{\link[stats]{character(0)}}
#' @rdname doubletCells
#' @export 
#' @importFrom stringr str_detect
#' @importFrom stats setNames 
doubletCells = function(m, clusters, samples, clusters_to_check = clusters, return.type = c('matrix', 'list')) {

    # clusters: list of cell ids per cluster
    # samples: character vector of unique sample ids
    # clusters_to_check: the cluster(s) that doubletCells should be tested in
    # this defaults to <clusters>, the clusters that doubletCells tests against

    # list with per-cell vectors of residual correlations to clusters
    results = list()

    stopifnot(is.list(clusters) && is.list(clusters_to_check))

    # for cluster id in clusters to test/check
    for (name in names(clusters_to_check)) {
        ref = clusters_to_check[[name]]
        clust.names = names(clusters)
        
        for (cell in ref) {
            # which sample is cell from?
            sample = extract_substring(cell, samples)
            # filter clusters_to_check cluster for sample-specific cell subset
            refi = ref[stringr::str_detect(ref, sample)]
            # same for clusters to check against 
            remnants = sapply(clusters, function(x) x[stringr::str_detect(x, sample)], simplify = F)
            # remove clusters with less than one cell (after sample-specific filtering)
            remnants = remnants[lengths(remnants) >= 1]
            # empty correlation vector of zeroes
            cors = stats::setNames(rep(0, length(clust.names)), clust.names)
            
            if (length(remnants) > 0) {
                # average expression profile of sample-specific reference cluster
                centroid = logRowMeans(m[,refi,drop = F])
                # cell residual against reference cluster
                resid = m[,cell] - centroid
                #rem.resids = sapply(remnants, function(x) logRowMeans(m[, x,drop = F]), simplify = F)
                rem.resids = sapply(remnants, function(x) logRowMeans(m[, x,drop = F]) - centroid, simplify = F)
                rem.resids = stats::setNames(as.data.frame(rem.resids), names(rem.resids))
                tmp = suppressWarnings(cor(resid, rem.resids))
                cors[colnames(tmp)] <- as.numeric(tmp)
            }

            results = c(results, setNames(list(cors), cell))
        }
        message('cluster <', name, '>: done')
    }

    switch(match.arg(return.type),
           matrix = do.call(cbind, results),
           list = results)
}

#m = as.matrix(readRDS('data/m.allgenes.logcpm.rds'))
#m = m[aggr_gene_expr(m) >= 4, ]
#m = rowcenter(m)
#clusters = readRDS('data/clusters_40.rds')[-28]
#samples = unique_sample_names(colnames(m))
#res = doubletCells(m, clusters, samples)

