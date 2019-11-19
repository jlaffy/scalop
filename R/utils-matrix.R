
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


has_dim <- function(x) {
  !is.null(attr(x, "dim"))
}

split_matrix = function(m, by) {
    stopifnot(has_dim(m))
    stopifnot(is.character(by))
    stopifnot(all(by %in% colnames(m)))
    list(x = m[, by], y = m[, !colnames(m) %in% by])
}

have_equal_nrows = function(m1, m2) {
    nrow(m1) == nrow(m2)
}

have_equal_rownames = function(m1, m2) {
    all(rownames(m1) == rownames(m2))
}

have_equal_dims = function(m) {
    nrow(m) == ncol(m)
}

is_square = have_equal_dims

is_cor = function(m) {
    rg = range(m)
    if ((is_square(m)) & (rg[1] >= -1) & (rg[2] <= 1)) {
        dg = unique(diag(m))
        return(length(dg) == 1 & dg == 1)
    }
    FALSE
}

is_symm = function(m) {
    (is_square(m)) && (sum(m == t(m)) != nrow(m)^2)
}

