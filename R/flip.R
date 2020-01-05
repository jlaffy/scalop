
#' @title Flip the (nested) elements of a character vector (or list) with its names
#' @description A convenience function to switch the names and elements of a named character vector or a named list of character vectors.
#' @param X A named character vector or a named list of character vectors.
#' @return A named character vector or a named list of character vectors. If the former, will be of the same length as <X>. If the latter, will be of the same length as there are unique elements across all vectors in the list.
#' @seealso 
#'  \code{\link[stats]{setNames}}
#' @rdname flip
#' @export 
#' @importFrom stats setNames
flip = function(X) {
    stopifnot(!is.null(names(X)))
    if (is.character(X)) {
        return(stats::setNames(names(X), X))
    }

    else if (is.list(X) & !has_dim(X)) {
        return(split(rep(names(X), lengths(X)), unlist(X, use.names = F)))
    }

    else stop('X should be a list or a character vector.')
}
