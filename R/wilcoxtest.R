#' @title wilcoxon-tests
#' @description wilcoxon-tests
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param ... adjust.method and cutoff
#' @return wilcoxon-tests
#' @rdname wilcoxtest
#' @export 
wilcoxtest = function(x, y, ...) {
    UseMethod('wilcoxtest', x)
}

wilcoxtest.NULL = function(...) {
    "NULL"
}

wilcoxtest.character = function(x, y, ...) {
    c(x, y) %<-% split_matrix(m = y, by = x)
    wilcoxtest.matrix(x = x, y = y, ...)
}

wilcoxtest.matrix = function(x, y, adjust.method = 'BH', cutoff = NULL, ...) {
    x = as.matrix(x)
    y = as.matrix(y)
    stopifnot(have_equal_rownames(x, y))
    res = sapply(1:nrow(x), function(i) stats::wilcox.test(x[i, ], y[i, ])$p.value)
    res = stats::setNames(stats::p.adjust(res, method = adjust.method), rownames(x))
    if (!is.null(cutoff) && is_p_value(cutoff)) res = res[res <= cutoff]
    res
}

wilcoxtest.data.frame = wilcoxtest.matrix

wilcoxtest.default = function(x, y, ...) {
    message('Class of <y> not recognised.')
}
