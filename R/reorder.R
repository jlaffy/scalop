
.reorder.cormat = function(m, col = T, row = T) {
    ord = hca(cr = m, ord = T)
    if (col) m = m[, ord]
    if (row) m = m[ord, ]
    m
}

#' @title Reorder matrix columns and rows
#' @description Reorder matrix columns and rows by hierarchical clustering of pairwise pearson correlation values
#' @param m a matrix
#' @param col logical indicating whether to reorder columns. Default: T
#' @param row logical indicating whether to reorder rows. Default: T
#' @return ordered matrix
#' @rdname reorder
#' @export 
reorder = function(m, col = T, row = T) {
    if (is.simil(m)) {
        return(.reorder.cormat(m, col = col, row = row))
    }
    if (col) m = m[, hca(m = m, ord = T)]
    if (row) m = m[hca(m = t(m), ord = T), ]
    m
}
