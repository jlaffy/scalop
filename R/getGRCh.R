
# function taken from TCGAbiolinks package

#' @title Get hg19 or hg38 information from biomaRt
#' @description Get hg19 or hg38 information from biomaRt
#' @param genome hg38 or hg19
#' @param as.granges Output as GRanges or data.frame
#' @importFrom biomaRt getBM useMart listDatasets useEnsembl
#' @export
getGRCh <- function(genome = "hg19", as.granges = FALSE) {
  tries <- 0L
  msg <- character()
  while (tries < 3L) {
    gene.location <- tryCatch({
      host <- ifelse(genome == "hg19", "grch37.ensembl.org",
                     "www.ensembl.org")
      mirror <- list(NULL, "useast", "uswest", "asia")[[tries + 1]]

      ensembl <- tryCatch({
        message(ifelse(is.null(mirror),
                       paste0("Accessing ", host, " to get gene information"),
                       paste0("Accessing ", host," (mirror ", mirror,")")))
        useEnsembl("ensembl", dataset = "hsapiens_gene_ensembl", host = host, mirror = mirror)
      }, error = function(e) {
        message(e)
        return(NULL)
      })

      attributes <- c("chromosome_name",
                      "start_position",
                      "end_position", "strand",
                      "ensembl_gene_id",
                      "entrezgene_id",
                      "external_gene_name")

      db.datasets <- listDatasets(ensembl)
      description <- db.datasets[db.datasets$dataset == "hsapiens_gene_ensembl",]$description
      message(paste0("Downloading genome information (try:", tries,") Using: ", description))

      chrom <- c(1:22, "X", "Y")
      gene.location <- getBM(attributes = attributes,
                             filters = c("chromosome_name"),
                             values = list(chrom), mart = ensembl)
      gene.location
    }, error = function(e) {
      msg <<- conditionMessage(e)
      tries <<- tries + 1L
      NULL
    })
    if(!is.null(gene.location)) break
  }
  if (tries == 3L) stop("failed to get URL after 3 tries:", "\n  error: ", msg)

  if(as.granges) {
    gene.location$strand[gene.location$strand == 1] <- "+"
    gene.location$strand[gene.location$strand == -1] <- "-"
    gene.location$chromosome_name <- paste0("chr",gene.location$chromosome_name)
    gene.location <- makeGRangesFromDataFrame(gene.location, seqnames.field = "chromosome_name",
                                              start.field = "start_position",
                                              end.field = "end_position",
                                              keep.extra.columns = TRUE) # considering the whole gene no their promoters
  }
  return(gene.location)
}
