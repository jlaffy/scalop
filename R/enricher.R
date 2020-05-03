
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param my_gene_sets PARAM_DESCRIPTION
#' @param my_universe PARAM_DESCRIPTION
#' @param p.adj PARAM_DESCRIPTION, Default: 0.01
#' @param term2gene PARAM_DESCRIPTION, Default: NULL
#' @param category PARAM_DESCRIPTION, Default: c("C2", "C5")
#' @param subcategory PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[msigdbr]{msigdbr}}
#'  \code{\link[clusterProfiler]{enricher}}
#' @rdname enricher
#' @export 
#' @importFrom msigdbr msigdbr
#' @importFrom clusterProfiler enricher
enricher = function(my_gene_sets,
                    my_universe,
                    p.adj = 0.01,
                    term2gene = NULL,
                    category = c('C2', 'C5'),
                    subcategory = NULL) {

    if (is.character(my_gene_sets)) {
        my_gene_sets = list(my_gene_sets)
    }

    if (is.null(term2gene)) {
        # term2gene
        sigs = sapply(category, function(s) {
                          msigdbr::msigdbr(category = s,
                                           species = 'Homo sapiens',
                                           subcategory = subcategory)},
                          simplify = F)

        sigs = sapply(sigs, function(df) df[, c(1, 5)], simplify = F)
        term2gene = do.call(rbind.data.frame, sigs)
    }

    # enrichment test
    result = sapply(my_gene_sets, function(my_gene_set) {
                        clusterProfiler::enricher(gene = my_gene_set,
                                                  universe = my_universe,
                                                  TERM2GENE = term2gene)},
                    simplify = F)

    sapply(result, as.data.frame, simplify = F) 
}

