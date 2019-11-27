
#' @export
gene_id2refseq = function(ids) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, ids, c("REFSEQ"), "ENTREZID")
}

#' @export
gene_refseq2many = function(ids) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, ids, c("ENTREZID", "SYMBOL"), "REFSEQ")
}

#' @export
gene_id2symbol = function(ids) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, ids, c("SYMBOL"), "ENTREZID")
}

#' @export
gene_symbol2id = function(symbols) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, symbols, c("ENTREZID"), "SYMBOL")
}

#' @export
gene_ensembl2many = function(a, atype = "ENSEMBL", btypes = c("SYMBOL", "ENTREZID")) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, a, btypes, atype)
}

#' @export
gene_symbol2many = function(a, atype = "SYMBOL", btypes = c("ENSEMBL", "ENTREZID")) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, a, btypes, atype)
}

#' @export
gene_id2many = function(a, atype = "ENTREZID", btypes = c("ENSEMBL", "SYMBOL")) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, a, btypes, atype)
}

gene_alias2many = function(a, atype = "ALIAS", btypes = c("ENSEMBL", "SYMBOL", "ENTREZID")) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, a, btypes, atype)
}


gene_atype2many = function(a, atype, btypes) {
    library(org.Hs.eg.db) 
    AnnotationDbi::select(org.Hs.eg.db, a, btypes, atype)
}
