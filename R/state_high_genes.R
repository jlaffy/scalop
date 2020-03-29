
#' @title Genes Associated with High Frequency of a Cellular State
#' @description The expression of genes in bulk samples reflects the combined effect from multiple expressing cell types and cell states, and therefore many genes that are good markers for a particular cellular state in single cell data may not be good markers in bulk data. To address this problem, this function identifies genes that are associated with high frequency of a particular cellular state. This analysis can highlight, amongst other things, genes whose sample-wide expression promotes particular cellular states.
#' @param scores a dataframe of cell signature scores. Cells are rows and signatures are columns. Samples or <groups> are split into state-high and state-low groups according to the states in <scores> columns.
#' @param groups a list of cell IDS by sample. 
#' @param scores2 a dataframe of cell signature scores. Cells are rows and signatures are columns. Cells within state-high and state-low groups are further subdivided into cellular states according to the columns in <scores2>, to allow for state-frequency controlled comparisons between state-high and state-low groups in bulk. In most cases, this can be left as the default : where the cellular state definitions are the same as provided by <scores>. Default: scores
#' @param min minimum value required for a cell (row in <scores>) to be assigned to a state (col in <scores>). Default: 0.5
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: NULL
#' @param gene.occurence the fraction of times a gene must be differentially expressed in order to be retained as a final high-freq-associated gene. Default: 0.5
#' @return a list of genes per state (as defined by the columns in <scores>) 
#' @details the function proceeds through the following steps: 1. we define state-high and state-low groups. This involves finding which samples have a significantly high, or significantly low, fraction of the cellular state in question. See `?scalop::assign_bulk_states` for details. 2. for each state, we perform differential expression analysis between the state-high samples and the state-low samples. To control for differing state frequencies between corresponding state-high and state-low groups, the differential expression analysis is performed for each cellular state in turn. Note that it is here that <scores2> is relevant. See `?scalop::dea` for details. 3. Per state-high - state-low comparison, we integrate the sets of DE genes (one set per cellular state) by keeping only those genes that appear across cellular states. You can modify across what fraction of the total number of cellular states a DE gene needs to be observed in order to be kept. See <gene.occurence> for details.
#' @rdname state_high_genes
#' @export 
state_high_genes = function(scores,
                            groups,
                            scores2 = scores,
                            gene.occurence = 0.5,
                            min = 0.5, 
                            diff = NULL) {

    library(scalop)
    sample.assig = assign_bulk_states(scores = scores,
                                      groups = groups,
                                      p = 1e-3,
                                      n.iter = 1e3,
                                      min = min,
                                      diff = diff,
                                      return.cells = TRUE)
    
    cell.assig = flip(maxcol_strict(scores2, min = min, diff = diff))
    # assig is a nested list by bulk-state, then by state-high and state-low, then by sc-state.
    assig = sapply(sample.assig, function(X) sapply(X, function(x) split(x, cell.assig[x]), simplify = F), simplify = F)
    
    .dea_matched = function(highFreq, lowFreq, m) {
        res0 = Map(dea,
                   group = highFreq,
                   group2 = lowFreq,
                   MoreArgs = list(m = m, return.val = 'lfc'))
    
        res0 = ldcast(res0)
        occ = rowMeans(!is.na(res0))
        res = res0[occ >= gene.occurence, ]
        occ = occ[occ >= gene.occurence]
        res = rowMeans(res, na.rm = TRUE)
        res = res[order(occ, res, decreasing = T)]
        res
    }
    
    sapply(assig, function(X) .dea_matched(highFreq = X$high, lowFreq = X$low, m = m))
}
