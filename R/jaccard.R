
.jaccard <- function(x, y) {
    length(intersect(x, y)) / length(union(x, y))
}

#' @title Compute Jaccard Similarities between Pairs of Character Vectors
#' @description This function computes pairwise Jaccard Similarities for all pairs of character vectors provided. Flexibility on input allows for three use cases: (1) In the simplest use case, two character vectors are provided to x and y arguments. (2) Alternatively, a single list of character vectors can be passed to x, in which case Jaccard similarities will be computed for all pairs of vectors in the list. (3) Lastly, two (different) lists of character vectors are provided to x and y arguments, respectively, in which case Jacccard similarities will be computed for all inter-list pairs of vectors.
#' @param x character vector or list of character vectors
#' @param y optional character vector or list of character vectors. If x is a character vector, y must be provided. Default: NULL
#' @return numeric Jaccard value between x and y if both are character vectors or a mrix of Jaccard values between all pairs of character vectors amongst x if x is a list and y was not provided, or between x and y if y was provided. If x and y were provided, the elements in x correspond to the columns in the output mrix.
#' @rdname jaccard
#' @export 
jaccard <- function(x, y = NULL, simplify = TRUE) {
    if (!is.null(dim(x))) {
        x = as.list(as.data.frame(x, stringsAsFactors = F))
    }
    
    if (is.null(y)) {
        y = x
    }

    if (!is.null(dim(y))) {
        y = as.list(as.data.frame(y, stringsAsFactors = F))
    }

    are.chars = sum(sapply(list(x, y), is.character)) == 2
    are.lists = sum(sapply(list(x, y), is.list)) == 2
    stopifnot(are.chars|are.lists)

    if (are.chars) {
        return(.jaccard(x = x, y = y))
    }

    comply(x, y, FUN = .jaccard)
}


#' @title Compute Jaccard Similarities between Pairs of Character Vectors
#' @description This function computes pairwise Jaccard Similarities for all pairs of character vectors provided. Flexibility on input allows for three use cases: (1) In the simplest use case, two character vectors are provided to x and y arguments. (2) Alternatively, a single list of character vectors can be passed to x, in which case Jaccard similarities will be computed for all pairs of vectors in the list. (3) Lastly, two (different) lists of character vectors are provided to x and y arguments, respectively, in which case Jacccard similarities will be computed for all inter-list pairs of vectors.
#' @param x character vector or list of character vectors
#' @param y optional character vector or list of character vectors. If x is a character vector, y must be provided. Default: NULL
#' @param cutoff remove rows/columns below the specified value. Default: 0
#' @param row boolean indicating whether to filter rows if cutoff is specified. Default: TRUE
#' @param col boolean indicating whether to filter cols if cutoff is specified. Default: FALSE
#' @return numeric Jaccard value between x and y if both are character vectors or a mrix of Jaccard values between all pairs of character vectors amongst x if x is a list and y was not provided, or between x and y if y was provided. If x and y were provided, the elements in x correspond to the columns in the output matrix.
#' @rdname Jaccard
#' @export 
Jaccard = function(x, y = NULL, cutoff = 0, row = T, col = F) {
	jac = jaccard(x, y)
	if (row) jac = jac[apply(jac, 1, function(row) any(row >= cutoff)),]
	if (col) jac = jac[, apply(jac, 2, function(row) any(row >= cutoff))]
	jac
}
