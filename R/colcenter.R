#' @title Center a matrix column-wise 
#' @description Center a matrix column-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of columns of ‘m’. Default: "mean"
#' @return column-centered matrix
#' @rdname colcenter
#' @export 
colcenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = T
    else if (by == 'median') by = matrixStats::colMedians(m)
    else stopifnot(is.numeric(by) & length(by) == ncol(m))
    scale(m, center = by, scale = F)
}
