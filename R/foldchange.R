#' @title fold changes
#' @description fold changes
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param ... is.log and cutoff
#' @return fold changes
#' @rdname foldchange
#' @export 
foldchange = function(x, y, ...) {
    UseMethod('foldchange', x)
}

foldchange.NULL = function(...) "NULL"

foldchange.default = function(x, y, ...) message('Class of <x> not recognised.')

foldchange.character = function(x, y, ...) {
    c(x, y) %<-% split_matrix(m = y, by = x)
    foldchange.matrix(x = x, y = y, ...)
}

foldchange.matrix = function(x, y, is.log = TRUE, cutoff = NULL, ...) {
    x = as.matrix(x)
    y = as.matrix(y)
    stopifnot(have_equal_rownames(x, y))
    if (!is.log) res = rowMeans(x)/rowMeans(y)
    else res = rowMeans(x) - rowMeans(y)
    if (is_number(cutoff)) {
        if (is.log) cutoff = log2(cutoff) 
        res = res[res >= cutoff]}
    res
}

foldchange.data.frame = foldchange.matrix

