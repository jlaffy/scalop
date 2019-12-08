
GeneInfo = function(symbols) {
# see https://davetang.org/muse/2013/12/16/bioconductor-annotation-packages/
    library(AnnotationDbi)
    columns = c('ENTREZID', 'GENENAME')
    info = select(org.Hs.eg.db::org.Hs.eg.db,
                  keys = symbols,
                  keytype = 'SYMBOL',
                  columns = columns)
    info = info[!duplicated(info$SYMBOL), ]
    info$GO = mapIds(org.Hs.eg.db::org.Hs.eg.db,
                     keys = info$SYMBOL,
                     keytype = 'SYMBOL',
                     column = 'GO')
    info$GOTERM = mapIds(GO.db::GO.db,
                         keys = info$GO,
                         keytype = 'GOID',
                         column = 'TERM')
    info$TXCHROM = mapIds(TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene,
                          keys = info$ENTREZID,
                          keytype = 'GENEID',
                          column = 'TXCHROM') 
    info
}
