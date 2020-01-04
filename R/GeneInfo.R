#' @title Get Genomic Information
#' @param symbols character vector of HGNC symbols
#' @export
#' @rdname GeneInfo
GeneInfo = function(symbols) {
# see https://davetang.org/muse/2013/12/16/bioconductor-annotation-packages/
    columns = c('ENTREZID', 'GENENAME')
    info = AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db,
                  keys = symbols,
                  keytype = 'SYMBOL',
                  columns = columns)
    info = info[!duplicated(info$SYMBOL), ]
    info$GO = AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                     keys = info$SYMBOL,
                     keytype = 'SYMBOL',
                     column = 'GO')
    info$GOTERM = AnnotationDbi::mapIds(GO.db::GO.db,
                         keys = info$GO,
                         keytype = 'GOID',
                         column = 'TERM')
    info$TXCHROM = AnnotationDbi::mapIds(TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene,
                          keys = info$ENTREZID,
                          keytype = 'GENEID',
                          column = 'TXCHROM') 
    info
}
