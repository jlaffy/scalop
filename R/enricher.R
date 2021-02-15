#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param my_gene_sets PARAM_DESCRIPTION
#' @param my_universe PARAM_DESCRIPTION
#' @param ref_gene_sets PARAM_DESCRIPTION
#' @param minGSSize PARAM_DESCRIPTION, Default: 10
#' @param maxGSSize PARAM_DESCRIPTION, Default: 500
#' @param pAdjustMethod PARAM_DESCRIPTION, Default: 'BH'
#' @param pvalueCutoff PARAM_DESCRIPTION, Default: 0.05
#' @param qvalueCutoff PARAM_DESCRIPTION, Default: 0.2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[limma]{alias2Symbol}}
#'  \code{\link[clusterProfiler]{enricher}}
#' @rdname enricher
#' @export 
#' @importFrom limma alias2Symbol
#' @importFrom clusterProfiler enricher
enricher = function(my_gene_sets,
		    my_universe,
		    ref_gene_sets,
		    minGSSize = 10,
		    maxGSSize = 500,
		    pAdjustMethod = "BH",
		    pvalueCutoff = 0.05,
		    qvalueCutoff = 0.2) {

    if (is.character(my_gene_sets)) {
        my_gene_sets = list(my_gene_sets)
    }
   
    my_gene_sets = sapply(my_gene_sets, function(s) limma::alias2Symbol(s), simplify = F)
    ref_gene_sets = sapply(ref_gene_sets, function(s) limma::alias2Symbol(s), simplify = F)
    my_universe = limma::alias2Symbol(my_universe)
    ref_universe = limma::alias2Symbol(unique(unlist(ref_gene_sets)))
    my_universe = intersect(my_universe, ref_universe)

    term2gene = data.frame(term = names(Unlist(ref_gene_sets)),
			   gene = Unlist(ref_gene_sets),
			   stringsAsFactors = F)

    # enrichment test
    result = sapply(my_gene_sets, function(gene_set) {
                        clusterProfiler::enricher(gene = gene_set,
                                                  universe = universe,
                                                  TERM2GENE = term2gene,
		    				  minGSSize = minGSSize,
		    				  maxGSSize = maxGSSize,
						  pAdjustMethod = pAdjustMethod,
		    				  pvalueCutoff = pvalueCutoff,
		    				  qvalueCutoff = pvalueCutoff)},
		    simplify = F)

    sapply(result, as.data.frame, simplify = F) 
}

