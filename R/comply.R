
#' @title Apply a Function to All Combinations in One or between a Pair of Vectors
#' @description Apply a Function to All Combinations in One or between a Pair of Vectors
#' @param x a vector or a list of vectors
#' @param y a vector or a list of vectors. If y is NULL, y will be set to x, such that if x is a list, the function FUN will be applied to all pairs of vectors in the list. Default: NULL
#' @param FUN the function to apply to all pairs of x and y. Default: jaccard
#' @return a matrix of resulting values after applying FUN to all pairs of x and y (or the one pair if x and y are both atomic vectors). The number of columns and the number of rows will correspond to the lengths of x and y respectively.
#' @seealso 
#'  \code{\link[stats]{setNames}}
#' @rdname comply
#' @export 
#' @importFrom stats setNames
comply = function(x, y = NULL, FUN = jaccard) {
    # map function FUN to all combinations of x and y
    # if y is null, y is set to x

    if (!is.null(dim(x))) {
        x = stats::setNames(as.list(as.data.frame(x)), colnames(x))
    }
    if (is.null(y)) {
        y = x
    }
    if (!is.null(dim(y))) {
        y = stats::setNames(as.list(as.data.frame(y)), colnames(y))
    }

    rows = names(y)
    colm = names(x)
    m = outer(seq_along(x),
              seq_along(y),
              FUN = Vectorize(function(i,j) FUN(x[[i]], y[[j]])))
    m = t(m)
    rownames(m) = rows
    colnames(m) = colm
    m
} 


