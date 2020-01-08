#' @title Convert CPM to logCPM 
#' @description Convert CPM to logCPM, i.e. using log2(CPM/10 + 1).
#' @param m matrix of CPM values (gene rows; cell columns)
#' @param bulk if bulk then instead uses log2(CPM + 1). i.e. no scaling. Default: F
#' @return logCPM matrix 
#' @details CPM/10 is used for single cells since 100,000 is a more reasonable estimate than 1,000,000 for the number of RNA transcripts in a cell. 1,000,000 is reasonable estimate for bulk samples that contain multiple cells.
#' @rdname logcpm
#' @export 
logcpm = function(m, bulk = F) {
    logtpm(m = m, bulk = bulk)
}

#' @title Convert logCPM to CPM 
#' @description Convert logCPM to CPM, i.e. using 10*(2^(CPM)-1).
#' @param m matrix of logCPM values (gene rows; cell columns)
#' @param bulk if bulk then instead uses 2^(CPM)-1. i.e. no scaling. Default: F
#' @return CPM matrix 
#' @details CPM/10 is used for single cells since 100,000 is a more reasonable estimate than 1,000,000 for the number of RNA transcripts in a cell. 1,000,000 is reasonable estimate for bulk samples that contain multiple cells.
#' @rdname unlogcpm
#' @export 
unlogcpm = function(m, bulk = F) {
    unlogtpm(m = m, bulk = bulk)
}

#' @title Convert Counts to Counts-Per-Million (CPM) 
#' @description Convert counts to CPM, by scaling raw counts such that they sum to 1 million per cell.
#' @param counts matrix of raw counts (gene rows; cell columns)
#' @return scaled matrix of counts-per-million values 
#' @rdname cpm
#' @export 
cpm = function(counts) {
    scale(counts, center = F, scale = colSums(counts)/1e6L)
}

