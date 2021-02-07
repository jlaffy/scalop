#' @title Center a matrix column-wise 
#' @description Center a matrix column-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of columns of ‘m’. Default: "mean"
#' @return column-centered matrix
#' @rdname colcenter
#' @export 
colcenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = colMeans(m, na.rm = T)
    else if (by == 'median') by = matrixStats::colMedians(m, na.rm = T)
    else stopifnot(is.numeric(by) & length(by) == ncol(m))
    scale(m, center = by, scale = F)
}

#' @title Center a matrix row-wise 
#' @description Center a matrix row-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of rows of ‘m’. Default: "mean"
#' @return row-centered matrix
#' @rdname rowcenter
#' @export 
rowcenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = rowMeans(m, na.rm = T)
    else if (by == 'median') by = matrixStats::rowMedians(m, na.rm = T)
    else stopifnot(is.numeric(by) & length(by) == nrow(m))
    t(scale(t(m), center = by, scale = F))
}

#' @title Number of non-zero values per column
#' @description Number of non-zero values per column
#' @param m matrix 
#' @return numeric vector
#' @seealso 
#'  \code{\link[matrixStats]{rowCounts}}
#'  \code{\link[stats]{setNames}}
#' @rdname coldetected
#' @export 
coldetected = function(m, value = 0,method = c('notequal','greaterthan','lessthan','equal'), counts = TRUE) {
    method = match.arg(method)
    m = as.matrix(m)
    if (method == 'notequal') {
        bool = m != value
        if (!counts) return(bool)
        res = matrixStats::colCounts(bool)
    }
    if (method == 'equal') {
        bool = m == value
        if (!counts) return(bool)
        res = matrixStats::colCounts(bool)
    }
    if (method == 'greaterthan') {
        bool = m > value
        if (!counts) return(bool)
        res = matrixStats::colCounts(bool)
    }
    if (method == 'lessthan') {
        bool = m < value
        if (!counts) return(bool)
        res = matrixStats::colCounts(bool)
    }
    stats::setNames(res, colnames(m))
}

#' @title Number of non-zero values per row
#' @description Number of non-zero values per row
#' @param m matrix 
#' @return numeric vector
#' @seealso 
#'  \code{\link[matrixStats]{rowCounts}}
#'  \code{\link[stats]{setNames}}
#' @rdname rowdetected
#' @export 
rowdetected = function(m, value = 0,method = c('notequal','greaterthan','lessthan','equal'), counts = TRUE) {
    method = match.arg(method)
    m = as.matrix(m)
    if (method == 'notequal') {
        bool = m != value
        if (!counts) return(bool)
        res = matrixStats::rowCounts(bool)
    }
    if (method == 'equal') {
        bool = m == value
        if (!counts) return(bool)
        res = matrixStats::rowCounts(bool)
    }
    if (method == 'greaterthan') {
        bool = m > value
        if (!counts) return(bool)
        res = matrixStats::rowCounts(bool)
    }
    if (method == 'lessthan') {
        bool = m < value
        if (!counts) return(bool)
        res = matrixStats::rowCounts(bool)
    }
    stats::setNames(res, rownames(m))
}

#' @title <dim> for many matrices
#' @description Returns the result of dim for every matrix in a list
#' @param mats a list of matrices (or a single matrix)
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
#' @param mats a list of matrices (or a single matrix)
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
#' @param mats a list of matrices (or a single matrix)
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

#' @export
has_dim <- function(x) {
    if (is.data.frame(x)) x = as.matrix(x)
    !is.null(attr(x, "dim"))
}

#' @export
split_matrix = function(m, by) {
    stopifnot(has_dim(m))
    stopifnot(is.character(by))
    stopifnot(all(by %in% colnames(m)))
    list(x = m[, by, drop = F], y = m[, !colnames(m) %in% by, drop = F])
}

#' @export
have_equal_nrows = function(m1, m2) {
    nrow(m1) == nrow(m2)
}

#' @export
have_equal_rownames = function(m1, m2) {
    all(rownames(m1) == rownames(m2))
}

#' @export
is_square = function(m) {
    nrow(m) == ncol(m)
}

#' @export
have_equal_dims = function(m1, m2) {
    identical(dim(m1), dim(m2))
}

#' @export
is_cor = function(m) {
    rg = range(m)
    if ((is_square(m)) & (rg[1] >= -1) & (rg[2] <= 1)) {
        dg = unique(diag(m))
        return(length(dg) == 1 & dg == 1)
    }
    FALSE
}

#' @export
is_symm = function(m) {
    (is_square(m)) && (sum(m == t(m)) == nrow(m)^2)
}

