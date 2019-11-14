#' @title Range of the rowMeans of a matrix
#' @description Returns the range over the rowMeans of a matrix
#' @param mat a matrix
#' @return the range of the rowMeans of the matrix provided.
#' @rdname range_rowmeans
#' @export 
range_rowmeans <- function(mat) {
    range(rowMeans(mat))
}

