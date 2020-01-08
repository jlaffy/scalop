
#' @export
gene_id2refseq = function(ids) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, ids, c("REFSEQ"), "ENTREZID")
}

#' @export
gene_refseq2many = function(ids) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, ids, c("ENTREZID", "SYMBOL"), "REFSEQ")
}

#' @export
gene_id2symbol = function(ids) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, ids, c("SYMBOL"), "ENTREZID")
}

#' @export
gene_symbol2id = function(symbols) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, symbols, c("ENTREZID"), "SYMBOL")
}

#' @export
gene_ensembl2many = function(a, atype = "ENSEMBL", btypes = c("SYMBOL", "ENTREZID")) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, a, btypes, atype)
}

#' @export
gene_symbol2many = function(a, atype = "SYMBOL", btypes = c("ENSEMBL", "ENTREZID")) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, a, btypes, atype)
}

#' @export
gene_id2many = function(a, atype = "ENTREZID", btypes = c("ENSEMBL", "SYMBOL")) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, a, btypes, atype)
}

gene_alias2many = function(a, atype = "ALIAS", btypes = c("ENSEMBL", "SYMBOL", "ENTREZID")) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, a, btypes, atype)
}


gene_atype2many = function(a, atype, btypes) {
    AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, a, btypes, atype)
}
