
#' @title Number of non-zero values per column
#' @description Number of non-zero values per column
#' @param m matrix 
#' @return numeric vector
#' @seealso 
#'  \code{\link[matrixStats]{rowCounts}}
#'  \code{\link[stats]{setNames}}
#' @rdname coldetected
#' @export 
#' @importFrom matrixStats colCounts
#' @importFrom stats setNames
coldetected = function(m) {
    m = as.matrix(m)
    res = matrixStats::colCounts(m != 0)
    stats::setNames(res, colnames(m))
}

