
#' @title Ordered correlation matrix
#' @description Computes correlation matrix and reorders its columns and rows by hierarchical clustering of the correlation matrix. 
#' @param m a matrix
#' @param col logical indicating whether to reorder columns. Default: T
#' @param row logical indicating whether to reorder rows. Default: T
#' @return ordered correlation matrix.
#' @rdname crorder
#' @export 
crorder = function(m, col = T, row = T) {
    if (is.cor(m)) {
        warning('Check that <m> is not already a correlation matrix...')
    }
    obj = hca(m = m)
    cr = obj$cr
    ord = obj$ord
    if (col) cr = cr[, ord]
    if (row) cr = cr[ord, ]
    cr
}


