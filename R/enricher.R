#' @title Functional enrichments 
#' @description Functional enrichments by hypergeometric test
#' @param test_gene_sets (list of) character vector gene IDs
#' @param ref_gene_sets (list of) character vector gene IDs
#' @param universe all genes considered. If NULL, uses all genes in test_gene_sets. Default: NULL
#' @param minGSSize minimum gene set size, Default: 10
#' @param maxGSSize maximum gene set size, Default: 500
#' @param pAdjustMethod p-value adjustment method, Default: 'BH'
#' @param pvalueCutoff p-value cutoff, Default: 0.05
#' @param qvalueCutoff FDR cutoff, Default: 0.2
#' @return dataframe with enrichment statistics.
#' @details enriched ref_gene_sets per test_gene_set are returned. test_gene_set names are specified in the name column and ref_gene_set name are specified in the ID column.
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
enricher = function(test_gene_sets,
		    ref_gene_sets,
		    universe = NULL,
		    minGSSize = 10,
		    maxGSSize = 500,
		    pAdjustMethod = "BH",
		    pvalueCutoff = 0.05,
		    qvalueCutoff = 0.2) {

    if (is.character(test_gene_sets)) {
        test_gene_sets = list(test_gene_sets)
    }
   
    test_gene_sets = sapply(test_gene_sets, function(s) limma::alias2Symbol(s), simplify = F)
    universe = limma::alias2Symbol(universe)
    ref_universe = unique(unlist(ref_gene_sets))
    universe = intersect(universe, ref_universe)
    test_gene_sets = sapply(test_gene_sets, function(x) x[x %in% universe], simplify = F)
    ref_gene_sets = sapply(ref_gene_sets, function(x) x[x %in% universe], simplify = F)

    term2gene = data.frame(term = names(Unlist(ref_gene_sets)),
			   gene = Unlist(ref_gene_sets),
			   stringsAsFactors = F)

    # enrichment test
    result = sapply(test_gene_sets, function(gene_set) {
                        clusterProfiler::enricher(gene = gene_set,
                                                  universe = universe,
                                                  TERM2GENE = term2gene,
		    				  minGSSize = minGSSize,
		    				  maxGSSize = maxGSSize,
						  pAdjustMethod = pAdjustMethod,
		    				  pvalueCutoff = pvalueCutoff,
		    				  qvalueCutoff = pvalueCutoff)},
		    simplify = F)

    sapply(1:length(result), function(i) {
		   as.data.frame(result[[i]]) %>% dplyr::mutate(name=rep(names(result)[i],nrow(result[[i]])))},
		   simplify = F) 
}

