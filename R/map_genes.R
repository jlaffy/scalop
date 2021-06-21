
### retrieve all genes/entries for a particular annotation type ###
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keytype PARAM_DESCRIPTION
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
#' @rdname get_genes
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_genes = function(keytype) {
	.genes = function() {
		org = Homo.sapiens::Homo.sapiens
		AnnotationDbi::keys(org, keytype=keytype)
	}
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

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
#' @rdname get_ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_ucsc=get_genes('UCSCKG')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

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
#' @rdname get_refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_refseq=get_genes('REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

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
#' @rdname get_symbols
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_symbols=get_genes('SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

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
#' @rdname get_ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_ensembl=get_genes('ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

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
#' @rdname get_entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
get_entrez=get_genes('ENTREZID')


### return annotation type ###
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keytype PARAM_DESCRIPTION
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
#' @rdname is_annotation
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_annotation = function(keytype) {
	.is_annotation = function(keys) {
		org = Homo.sapiens::Homo.sapiens
		universe = AnnotationDbi::keys(org, keytype=keytype)
		keys %in% universe
	}
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_symbol = is_annotation('SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_entrez = is_annotation('ENTREZID')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_ensembl = is_annotation('ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_refseq = is_annotation('REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_accnum
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_accnum = is_annotation('ACCNUM')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_alias = is_annotation('ALIAS')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
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
#' @rdname is_ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys
is_ucsc = is_annotation('UCSCKG')


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION
#' @param aliasesBeforeSymbols PARAM_DESCRIPTION, Default: FALSE
#' @param priority PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which_annotation
#' @export 
which_annotation = function(keys, aliasesBeforeSymbols=FALSE, priority=NULL) {
	keytypes = c('SYMBOL',
		     'ALIAS',
		     'ENTREZID',
		     'ENSEMBL',
		     'REFSEQ',
		     'ACCNUM',
		     'UCSCKG',
		     'TXID',
		     'ENSEMBLTRANS',
		     'ENSEMBLPROT',
		     'UNIPROT',
		     'UNIGENE',
		     'EXONID',
		     'ENZYME')

	if (aliasesBeforeSymbols) {
		if (!is.null(priority) && priority!='SYMBOL') keytypes[1:2] <- keytypes[2:1]
		else message("You set priority to 'SYMBOL' so aliasesBeforeSymbols will be ignored.")
	}

	if (!is.null(priority)) {
		if (!priority %in% keytypes) stop(priority, ' does not exist')
		else keytypes = c(priority, setdiff(keytypes,priority))
	}

	.call = function(keytype) {
		if (any(is.na(nams))) {nams[is.na(nams) & is_annotation(keytype)(keys)] <<- keytype}
	}

	nams = rep(NA,length(keys))
	sapply(keytypes, .call)
	setNames(nams, keys)
}


### map genes from one annotation type to others and return DF ###
### default return columns: SYMBOL, ENTREZID, TXCHROM ###
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keytype PARAM_DESCRIPTION
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
#' @rdname map_genes
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_genes = function(keytype) {
	.map_genes = function(keys=NULL, columns=NULL, add.columns=NULL, remove.columns=NULL) {
		org = Homo.sapiens::Homo.sapiens
		if (is.null(columns)) columns = c('SYMBOL','ENTREZID','TXCHROM')
		columns = setdiff(columns,keytype)
		columns = setdiff(columns,remove.columns)
		columns = union(columns,add.columns)
	
		if (is.null(keys)) {
			keys = AnnotationDbi::keys(org, keytype=keytype)
		}
	
		AnnotationDbi::select(org,keys=keys,keytype=keytype,columns=columns)
	}
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param add.columns PARAM_DESCRIPTION, Default: NULL
#' @param remove.columns PARAM_DESCRIPTION, Default: NULL
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
#' @rdname map_entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_entrez = map_genes('ENTREZID')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param add.columns PARAM_DESCRIPTION, Default: NULL
#' @param remove.columns PARAM_DESCRIPTION, Default: NULL
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
#' @rdname map_symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_symbol = map_genes('SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param add.columns PARAM_DESCRIPTION, Default: NULL
#' @param remove.columns PARAM_DESCRIPTION, Default: NULL
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
#' @rdname map_ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_ensembl = map_genes('ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param add.columns PARAM_DESCRIPTION, Default: NULL
#' @param remove.columns PARAM_DESCRIPTION, Default: NULL
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
#' @rdname map_refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_refseq = map_genes('REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param add.columns PARAM_DESCRIPTION, Default: NULL
#' @param remove.columns PARAM_DESCRIPTION, Default: NULL
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
#' @rdname map_alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi keys select
map_alias = map_genes('ALIAS')


### convert between two keytypes ### 
### this I think passes through ENTREZID ###
### if multiVals ='first', returns smallest ENTREZID
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
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
#' @rdname x2y
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
x2y = function(x,y) {
	.x2y = function(keys=NULL,multiVals='first') {
		if (is.null(keys)) {
			keys = get_genes(x)()
		}
		org = Homo.sapiens::Homo.sapiens
		out=rep(NA,length(keys))
		is.ready = is_annotation(y)(keys)
		out[is.ready] <- keys[is.ready]
		bool=!is.ready & is_annotation(x)(keys)
		if (any(bool)) {
			out[bool]<-AnnotationDbi::mapIds(org,
							 keys=keys[bool],
							 keytype=x,
							 column=y,
							 multiVals=multiVals)
		}
		out
	}
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname alias2symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
alias2symbol = x2y('ALIAS','SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname alias2entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
alias2entrez = x2y('ALIAS','ENTREZID')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname alias2ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
alias2ucsc = x2y('ALIAS','UCSCKG')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname alias2refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
alias2refseq = x2y('ALIAS','REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname alias2ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
alias2ensembl = x2y('ALIAS','ENSEMBL')

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname symbol2entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
symbol2entrez = x2y('SYMBOL','ENTREZID')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname symbol2ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
symbol2ensembl = x2y('SYMBOL','ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname symbol2refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
symbol2refseq = x2y('SYMBOL','REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname symbol2ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
symbol2ucsc = x2y('SYMBOL','UCSCKG')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname symbol2alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
symbol2alias = x2y('SYMBOL','ALIAS')

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname entrez2symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
entrez2symbol = x2y('ENTREZID','SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname entrez2ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
entrez2ensembl = x2y('ENTREZID','ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname entrez2refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
entrez2refseq = x2y('ENTREZID','REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname entrez2ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
entrez2ucsc = x2y('ENTREZID','UCSCKG')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname entrez2alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
entrez2alias = x2y('ENTREZID','ALIAS')

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname refseq2entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
refseq2entrez = x2y('REFSEQ','ENTREZID')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname refseq2symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
refseq2symbol = x2y('REFSEQ','SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname refseq2ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
refseq2ensembl = x2y('REFSEQ','ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname refseq2ucsc
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
refseq2ucsc = x2y('REFSEQ','UCSCKG')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname refseq2alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
refseq2alias = x2y('REFSEQ','ALIAS')

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname ucsc2symbol
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
ucsc2symbol = x2y('UCSCKG','SYMBOL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname ucsc2ensembl
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
ucsc2ensembl = x2y('UCSCKG','ENSEMBL')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname ucsc2refseq
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
ucsc2refseq = x2y('UCSCKG','REFSEQ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname ucsc2entrez
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
ucsc2entrez = x2y('UCSCKG','ENTREZ')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param keys PARAM_DESCRIPTION, Default: NULL
#' @param multiVals PARAM_DESCRIPTION, Default: 'first'
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
#' @rdname ucsc2alias
#' @export 
#' @importFrom Homo.sapiens Homo.sapiens
#' @importFrom AnnotationDbi mapIds
ucsc2alias = x2y('UCSCKG','ALIAS')
