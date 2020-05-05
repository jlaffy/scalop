 
# var2char <- function(v1) {
#   deparse(substitute(v1))
# }

# retrieve vector of clust ids (with cell names -- column names in x)
# with option to return as a dataframe of two columns: cell and clust
# x can be expression matrix of genes by cells
# common to use reduced dims instead of genes
# eg. (50) PCs by cells ; the output of p
# or a dimred (e.g. PCA) matrix: e.g. 50 PCs (instead of genes)by cells
# for example PCA result: 50 PCs by cells
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 10
#' @param d PARAM_DESCRIPTION, Default: nrow(m)
#' @param return.list PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[scran]{buildSNNGraph}}
#'  \code{\link[igraph]{cluster_walktrap}}
#' @rdname get_snn_graph_clusters
#' @export 
#' @importFrom scran buildSNNGraph
#' @importFrom igraph cluster_walktrap
get_snn_graph_clusters = function(m,
                                  k = 10,
                                  d = nrow(m),
                                  return.list = FALSE) {

    # build SNN graph
    g <- scran::buildSNNGraph(x = m, k = k, d = d)

    # apply walktrap community clustering to SNN graph
    # and retrieve vec of cluster ids
    # same order as cell column names in x
    clust <- igraph::cluster_walktrap(g)$membership

    # set cluster ids
    names(clust) <- colnames(m)

    print(table(clust))

    if (return.list) {
        clust = split(names(clust), clust)
    }

    clust
}



## retrieve vector of clust ids (with cell names -- column names in x)
## and add as column to dataframe
## expects 'cell' column in dataframe DF
## and expects that all cell names exist in DF$cell
#add_snn_graph_clusters_to_DF = function(m, 
#                                        k = 10,
#                                        d = 50,
#                                        DF) {
#
#    stopifnot('cell' %in% colnames(DF))
#    stopifnot(all(colnames(x) %in% DF$cell))
#
#    clust = get_snn_graph_clusters(m = m,
#                                   k = k,
#                                   d = d,
#                                   asDF = FALSE)
#
#    DF$clust <- as.factor(clust[df$cell])
#    DF
#}
