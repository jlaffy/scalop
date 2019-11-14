## approximate equality
about_equal = function(x,y,tol=1e-10) {
    stopifnot(is.numeric(x), is.numeric(y), length(x)==length(y), all(abs(x-y) < tol))
}

#' @title <dim> for many matrices
#' @description Returns the result of dim for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return dim for each matrix provided.
#' @rdname dims
#' @export 
dims <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(dim(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, dim, simplify = T)
}


#' @title <ncol> for many matrices
#' @description Returns the result of ncol for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return ncol for each matrix provided.
#' @rdname ncols
#' @export 
ncols <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(ncol(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, ncol, simplify = T)
}


#' @title <nrow> for many matrices
#' @description Returns the result of nrow for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return nrow for each matrix provided.
#' @rdname nrows
#' @export 
nrows <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(nrow(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, nrow, simplify = T)
}

is.cor = function(m) {
    m = as.matrix(m)
    rg = range(m)
    is.simil(m) & rg[1] >= -1 & rg[2] <= 1
}

is.simil = function(m) {
    m = as.matrix(m)
    matrixcalc::is.symmetric.matrix(m)
}

sort_by = function(..., which = 1, decreasing = T) {
    dots = list(...)
    if (any(which > length(dots))) stop('<which> vector index is larger than the number of vectors.')
    orderers = sapply(which, function(i) dots[[i]], simplify = F)
    Order = do.call(order, c(orderers, list(decreasing = decreasing)))
    sapply(1:length(dots), function(i) dots[[i]][Order], simplify = F)
}


is_p_value = function(x) {
    !is.null(x) && is.numeric(x) && x >= 0 & x <= 1
}

have_equal_nrows = function(m1, m2) {
    nrow(m1) == nrow(m2)
}

have_equal_rownames = function(m1, m2) {
    all(rownames(m1) == rownames(m2))
}

has_dim <- function(x) {
  !is.null(attr(x, "dim"))
}

split_matrix = function(m, by) {
    stopifnot(has_dim(m))
    stopifnot(is.character(by))
    stopifnot(all(by %in% colnames(m)))
    list(x = m[, by], y = m[, !colnames(m) %in% by])
}

is_number = function(x) {
    is.numeric(x) & length(x) == 1
}


#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%->\%}} for details.
#' @importFrom zeallot %->%
#' @export
#' @rdname unpack-assign-back
#' @name %->%
#' @keywords internal
`%->%`

#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%<-\%}} for details.
#' @importFrom zeallot %<-%
#' @export
#' @rdname unpack-assign
#' @name %<-%
#' @keywords internal
`%<-%`
