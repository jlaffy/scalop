#' @title Center a matrix row-wise 
#' @description Center a matrix row-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of rows of ‘m’. Default: "mean"
#' @return row-centered matrix
#' @rdname rowcenter
#' @export 
rowcenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = T
    else if (by == 'median') by = matrixStats::rowMedians(m)
    else stopifnot(is.numeric(by) & length(by) == nrow(m))
    t(scale(t(m), center = by, scale = F))
}

