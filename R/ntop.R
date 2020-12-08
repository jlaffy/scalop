
#' @title Select top n elements 
#' @description ntop() returns the top 'n' elements of a vector or per vector in a list.
#' @param x A vector or list.
#' @param n Number of elements to return.
#' @param remove.lower Remove vectors that have fewer than 'n' elements. Default: FALSE
#' @return A vector or list of the same type as 'x', with a maximum of 'n' elements per vector.
#' @rdname ntop
#' @export 
ntop = function(x, n, remove.lower = FALSE) {
    if (remove.lower) {
        x = x[lengths(x) >= n]
    }

    sapply(x, `[`, 1:n, simplify = F) %>%
        sapply(., function(x) x[!is.na(x)], simplify = F)
}
