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

