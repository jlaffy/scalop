
#' @title Unlist, keeping original list or element names
#' @description Unlist, keeping original list or element names
#' @param L list to flatten
#' @param nested.names logical; keep nested list names rather than expanding list names. Default: FALSE
#' @return a vector of the same length as the combined lengths of list elements. Names will either be the list names replicated, or, if nested.names is TRUE, the original list element names will be kept.
#' @seealso 
#'  \code{\link[stats]{setNames}}
#' @rdname Unlist
#' @export 
#' @importFrom stats setNames
Unlist = function(L, nested.names = FALSE) {
    if (nested.names) {
        Names = unlist(sapply(L, names), use.names = F)
    } else {
        Names = rep(names(L), lengths(L))
    }
    stats::setNames(unlist(L), Names)
}
