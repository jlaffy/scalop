#' @title Drop columns whose names match a pattern
#' @description col_exclude() is a wrapper around m[, !str_detect(colnames(m), pattern)]. Equivalent to col_subset() with negate = TRUE.
#' @param m Input matrix or data frame.
#' @param pattern Pattern to look for. The default interpretation is a regular expression, as described in stringi::stringi-search-regex. Control options with 'regex()'.
#' @return A matrix.
#' @details Vectorised over colnames('m') and 'pattern'
#' @rdname col_exclude
#' @export
col_exclude = function(m, pattern) {
    pattern = unlist(pattern)
    if (length(pattern) > 1) {
        pattern = pattern[order(sapply(pattern, nchar), decreasing = T)]
        pattern = paste0(pattern, collapse = "|")
    }
    m[, !str_detect(colnames(m), pattern), drop = F]
}
