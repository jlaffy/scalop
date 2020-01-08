
#' @title Apply a Function to All Pairs of Elements in 'x' or Between 'x' and 'y' 
#' @description Apply a Function to All Pairs of Elements in 'x' or Between 'x' and 'y' 
#' @param x a vector or a list of vectors
#' @param y a vector or a list of vectors. If y is NULL, y will be set to x, such that if x is a list, the function FUN will be applied to all pairs of vectors in the list. Default: NULL
#' @param FUN the function to be applied to every pair of elements in 'x', or to every pair of elements between 'x' and 'y'. The function should take two arguments and return a numeric value. Default: jaccard
#' @param ... optional arguments to 'FUN'.
#' @param simplify logical; should the result be simplified to a matrix? Default: TRUE
#' @return a matrix of resulting values after applying FUN to all pairs of x and y (or the one pair if x and y are both atomic vectors). The number of columns and the number of rows will correspond to the lengths of x and y respectively.
#' @rdname comply
#' @export 
comply = function(x, y = NULL, FUN = jaccard, ..., simplify = TRUE) {
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
              FUN = Vectorize(function(i,j) FUN(x[[i]], y[[j]], ...)))

    if (is.matrix(m)) {
        m = t(m)
        rownames(m) = rows
        colnames(m) = colm
    }

    if (!simplify && is.matrix(m)) {
        m = as.list(as.data.frame(m))
    }

    m
} 


