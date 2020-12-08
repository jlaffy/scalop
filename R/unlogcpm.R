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

