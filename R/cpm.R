#' @title Convert Counts to Counts-Per-Million (CPM) 
#' @description Convert counts to CPM, by scaling raw counts such that they sum to 1 million per cell.
#' @param counts matrix of raw counts (gene rows; cell columns)
#' @return scaled matrix of counts-per-million values 
#' @rdname cpm
#' @export 
cpm = function(counts) {
    scale(counts, center = F, scale = colSums(counts)/1e6L)
}

