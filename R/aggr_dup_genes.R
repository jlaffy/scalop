### summary statistics for dealing with duplicated gene names in exprssion matrix

#' @title Aggregate duplicate gene rows
#' @description Aggregate duplicate gene rows. Duplicate rows are collapsed into one by taking the colSums, colMeans, colMaxs or colMedians.
#' @param m matrix with genes as rows
#' @param stat statistic to aggregate duplicate gene rows. Defaults to 'sum'. Default: c("sum", "mean", "max", "median")
#' @return matrix with unique gene rows
#' @seealso 
#'  \code{\link[matrixStats]{rowSums2}},\code{\link[matrixStats]{rowMeans2}},\code{\link[matrixStats]{rowRanges}},\code{\link[matrixStats]{rowMedians}}
#' @rdname aggr_dup_genes
#' @export 
#' @importFrom matrixStats colSums2 colMeans2 colMaxs colMedians
aggr_dup_genes = function(m, stat=c('sum','mean','max','median'), na.rm=T) {
  
  aggr_dup_gene = function(gene) {
    mgene = m[rownames(m)==gene,]
    statFUN(mgene, na.rm=na.rm)
  }
  
  statFUN = switch(match.arg(stat),
                   sum = matrixStats::colSums2,
                   mean = matrixStats::colMeans2,
                   max = matrixStats::colMaxs,
                   median = matrixStats::colMedians)
  
  one_col=FALSE
  if (ncol(m)==1) {
	  one_col=TRUE
	  m = cbind(m, m)
  }
  # gene order, keeping only first instance of dup.genes
  gene.ord = rownames(m)[!duplicated(rownames(m))]
  # find dup genes
  dup.genes = unique(rownames(m)[duplicated(rownames(m))])
  # for each dup genes, apply summary statistic and return numeric vec
  # rbind dup gene vectors
  mNew = do.call(rbind, sapply(dup.genes, aggr_dup_gene, simplify = F))
  m = m[!rownames(m) %in% dup.genes, ]
  m = rbind(m, mNew)
  #m = m[gene.ord,]
  if(one_col) {
	  m = m[,1,drop=F]
  }
  m
}
