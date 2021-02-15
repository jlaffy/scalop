#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param category PARAM_DESCRIPTION, Default: c("H", "C2", "C5")
#' @param subcategory PARAM_DESCRIPTION, Default: NULL
#' @param exclude.subcategory PARAM_DESCRIPTION, Default: 'HPO'
#' @param split.by.subcategory PARAM_DESCRIPTION, Default: F
#' @param species PARAM_DESCRIPTION, Default: 'Homo sapiens'
#' @param annotation PARAM_DESCRIPTION, Default: c("gene_symbol", "entrez_gene")
#' @param return.dataframe PARAM_DESCRIPTION, Default: F
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
#'  \code{\link[dplyr]{filter}}
#' @rdname msigdb
#' @export 
#' @importFrom msigdbr msigdbr
#' @importFrom dplyr filter
msigdb = function(category = c('H', 'C2', 'C5'),
		  subcategory = NULL,
		  exclude.subcategory = 'HPO', # Human Phenotype Ontology (in GO)
		  split.by.subcategory = F,
		  species = 'Homo sapiens',
		  annotation = c('gene_symbol','entrez_gene'),
		  return.dataframe = F) {

	annotation = match.arg(annotation)

	result = list()

	for (cat in category) {
		d = msigdbr::msigdbr(category = cat, species = species)
		d = d %>% dplyr::filter(!gs_subcat %in% exclude.subcategory)

		if (!is.null(subcategory) && any(subcategory %in% d$gs_subcat)) {
			d = d %>% dplyr::filter(gs_subcat %in% subcategory)
		}

		if (split.by.subcategory) {
			dl = split(d, d$gs_subcat)
		} else {
			dl = list(d)
		}

		if (return.dataframe) sigs = dl

		else {
			sigs = sapply(dl, function(di) {
					      split(di[[annotation]], di$gs_name)},
					      simplify = F)

			# just in case.
			sigs = sapply(sigs, function(s) s[!duplicated(s)], simplify = F)
		}

		result = c(result, setNames(sigs, cat))
	}

	result
}
