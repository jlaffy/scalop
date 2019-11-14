#' @title Number of non-zero values per row
#' @description Number of non-zero values per row
#' @param m matrix 
#' @return numeric vector
#' @seealso 
#'  \code{\link[matrixStats]{rowCounts}}
#'  \code{\link[stats]{setNames}}
#' @rdname rowdetected
#' @export 
#' @importFrom matrixStats rowCounts
#' @importFrom stats setNames
rowdetected = function(m) {
    m = as.matrix(m)
    res = matrixStats::rowCounts(m != 0)
    stats::setNames(res, rownames(m))
}
