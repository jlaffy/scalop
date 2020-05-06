
#' @title SNN Graph-Based Clustering
#' @description We first build a SNN graph, where each node is a cell connected to its neighbours in high dimensional space. Input multiple values for 'k' and 'ndim' -- the nearest neighbours and number of dimensions to use --- and the function will generate a graph for every combination. We then apply a clustering method to each of the graph(s) to identify clusters of cells. You can choose multiple clustering methods and the function will assign cells to clusters based on each method and for each graph. The clustering methods are taken from the igraph package. For a good and practical introduction of graph-based clustering, see: https://osca.bioconductor.org/clustering.html#clustering-graph
#' @param m matrix of features (genes/principal components) by observations (cells)
#' @param all.methods logical value. Methods: walktrap, louvain, infomap, fast_greedy, label_prop, leading_edge. See the igraph package for details. Default: FALSE
#' @param methods one or more clustering methods to use. If multiple, each will be run. Default: c("walktrap", "louvain")
#' @param k number of nearest neighbours to use in the graph construction. If multiple values are inputted, each will be run. Default: c(10, 30, 50)
#' @param ndim number of dimensions to use in the graph construction. If multiple values are inputted, each will be run. Default: nrow(m)
#' @param ... other arguments for graph construction passed to scran::buildSNNGraph
#' @return a dataframe
#' @details The function provides a friendly interface for trying several combinations of parameters for graph construction and cluster identification. The output is a dataframe with columns: id, k, ndim and one column per clustering method.
#' @seealso 
#'  \code{\link[igraph]{cluster_walktrap}},\code{\link[igraph]{cluster_louvain}},\code{\link[igraph]{cluster_infomap}},\code{\link[igraph]{cluster_fast_greedy}},\code{\link[igraph]{cluster_label_prop}},\code{\link[igraph]{cluster_leading_eigen}}
#'  \code{\link[scran]{buildSNNGraph}}
#'  \code{\link[dplyr]{join}}
#' @rdname ohmySNNclusters
#' @export 
#' @importFrom igraph cluster_walktrap cluster_louvain cluster_infomap cluster_fast_greedy cluster_label_prop cluster_leading_eigen
#' @importFrom scran buildSNNGraph
#' @importFrom dplyr full_join
ohmySNNclusters = function(m,
                           all.methods = FALSE,
                           methods = c('walktrap','louvain'),
                           k = c(10, 30, 50),
                           ndim = nrow(m),
                           ...) {

    # apply walktrap (or other) community clustering to SNN graph <g>
    # and retrieve vec of cluster ids
    # same order as cell column names in x
    funCalls = list(
        leading_eigen = quote(igraph::cluster_leading_eigen(g)$membership),
        label_prop = quote(igraph::cluster_label_prop(g)$membership),
        fast_greedy = quote(igraph::cluster_fast_greedy(g)$membership),
        infomap = quote(igraph::cluster_infomap(g)$membership),
        louvain = quote(igraph::cluster_louvain(g)$membership),
        walktrap = quote(igraph::cluster_walktrap(g)$membership)
    )


    if (all.methods) methods = names(funCalls)
    if (!all(methods %in% names(funCalls))) stop()
    funCalls = funCalls[names(funCalls) %in% methods]

    # make sure k (# nearest neighbours) isn't bigger than number of possible neighbours
    k = k[k < ncol(m)]
    k = as.list(k)

    # make sure that dimensions less or equal to those in <m>
    ndim = ndim[ndim <= nrow(m)]
    ndim = as.list(ndim)

    out = list()

    for (d in ndim) {
        for (ki in k) {
            # build SNN graph
            g = scran::buildSNNGraph(x = m, k = ki, d = d, ...)
            dat = sapply(funCalls, eval, envir = environment(), simplify = F)
            dat = do.call(cbind.data.frame, dat)
            dat$ndim = rep(d, nrow(dat))
            dat$k = rep(ki, nrow(dat))
            dat$id = colnames(m)
            out = c(out, list(dat))
        }
    }

    out = suppressMessages(Reduce(dplyr::full_join, out))
    out = out[, ncol(out):1]
    out
}
    

