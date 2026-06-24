#' @title Drop rows whose names match a pattern
#' @description row_exclude() is a wrapper around m[!str_detect(rownames(m), pattern), ]. Equivalent to row_subset() with negate = TRUE.
#' @param m Input matrix or data frame.
#' @param pattern Pattern to look for. The default interpretation is a regular expression, as described in stringi::stringi-search-regex. Control options with 'regex()'.
#' @return A matrix.
#' @details Vectorised over rownames('m') and 'pattern'
#' @rdname row_exclude
#' @export
row_exclude = function(m, pattern) {
    pattern = unlist(pattern)
    if (length(pattern) > 1) {
        pattern = pattern[order(sapply(pattern, nchar), decreasing = T)]
        pattern = paste0(pattern, collapse = "|")
    }
    m[!str_detect(rownames(m), pattern), , drop = F]
}
