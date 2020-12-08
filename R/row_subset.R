#' @title Keep rows whose names match a pattern 
#' @description row_subset() is a wrapper around m[, str_detect(rownames(m), pattern)]
#' @param m Input matrix or data frame.
#' @param pattern Pattern to look for. The default interpretation is a regular expression, as described in stringi::stringi-search-regex. Control options with 'regex()'. 
#' @param negate If 'TRUE', return non-matching rows.
#' @return A matrix.
#' @details Vectorised over rownames('m') and 'pattern'
#' @rdname row_subset
#' @export 
row_subset = function(m, pattern, negate = FALSE) {
    pattern = unlist(pattern)
    if (length(pattern) > 1) {
        pattern = pattern[order(sapply(pattern, nchar), decreasing = T), ]
        pattern = paste0(pattern, rowlapse = "|")
    }
    if (negate) {
        m = m[!str_detect(rownames(m), pattern), , drop = F]
    } else {
        m = m[, str_detect(rownames(m), pattern), , drop = F]
    }
    m
}
