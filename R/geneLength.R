#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param genes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[Homo.sapiens]{Homo.sapiens}}
#'  \code{\link[AnnotationDbi]{AnnotationDb-objects}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{rename}}
#' @rdname allowedExons
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi select
#' @importFrom dplyr filter group_by summarize rename
allowedExons = function(genes = NULL) {
	org = Homo.sapiens::Homo.sapiens
	canonical.chr=paste0('chr', c(1:22,'X','Y'))
	if (is.null(genes)) genes = keys(org, keytype='SYMBOL')
	out = AnnotationDbi::select(org,
			            keys=genes,
			            keytype='SYMBOL',
			            columns=c('EXONID','EXONRANK','EXONCHROM','EXONSTART','EXONEND')) %>%
	dplyr::filter(EXONCHROM %in% canonical.chr) %>%
	dplyr::group_by(EXONCHROM,SYMBOL) %>%
	dplyr::summarize(n_allowed_exons = length(unique(EXONRANK)),
			 n_total_exons = length(unique(EXONID)),
			 exon_start = min(EXONSTART),
			 exon_end = max(EXONEND),
			 gene_length = exon_end + 1 - exon_start) %>%
	dplyr::rename(gene = SYMBOL, exon_chr = EXONCHROM)

	setNames(out$n_allowed_exons, out$gene)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param genes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[Homo.sapiens]{Homo.sapiens}}
#'  \code{\link[AnnotationDbi]{AnnotationDb-objects}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{rename}}
#' @rdname totalExons
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi select
#' @importFrom dplyr filter group_by summarize rename
totalExons = function(genes = NULL) {
	org = Homo.sapiens::Homo.sapiens
	canonical.chr=paste0('chr', c(1:22,'X','Y'))
	if (is.null(genes)) genes = keys(org, keytype='SYMBOL')
	out = AnnotationDbi::select(org,
			            keys=genes,
			            keytype='SYMBOL',
			            columns=c('EXONID','EXONRANK','EXONCHROM','EXONSTART','EXONEND')) %>%
	dplyr::filter(EXONCHROM %in% canonical.chr) %>%
	dplyr::group_by(EXONCHROM,SYMBOL) %>%
	dplyr::summarize(n_allowed_exons = length(unique(EXONRANK)),
			 n_total_exons = length(unique(EXONID)),
			 exon_start = min(EXONSTART),
			 exon_end = max(EXONEND),
			 gene_length = exon_end + 1 - exon_start) %>%
	dplyr::rename(gene = SYMBOL, exon_chr = EXONCHROM)

	setNames(out$n_total_exons, out$gene)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param genes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[Homo.sapiens]{Homo.sapiens}}
#'  \code{\link[AnnotationDbi]{AnnotationDb-objects}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{rename}}
#' @rdname geneLength
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi select
#' @importFrom dplyr filter group_by summarize rename
geneLength = function(genes = NULL) {
	org = Homo.sapiens::Homo.sapiens
	canonical.chr=paste0('chr', c(1:22,'X','Y'))
	if (is.null(genes)) genes = keys(org, keytype='SYMBOL')
	out = AnnotationDbi::select(org,
			            keys=genes,
			            keytype='SYMBOL',
			            columns=c('EXONID','EXONRANK','EXONCHROM','EXONSTART','EXONEND')) %>%
	dplyr::filter(EXONCHROM %in% canonical.chr) %>%
	dplyr::group_by(EXONCHROM,SYMBOL) %>%
	dplyr::summarize(n_allowed_exons = length(unique(EXONRANK)),
			 n_total_exons = length(unique(EXONID)),
			 exon_start = min(EXONSTART),
			 exon_end = max(EXONEND),
			 gene_length = log10(exon_end + 1 - exon_start)) %>%
	dplyr::rename(gene = SYMBOL, exon_chr = EXONCHROM)

	setNames(out$gene_length, out$gene)
}
