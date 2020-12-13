
#' @title List combinations 
#' @description Combine all pairs of list elements
#' @param ... Unnamed list(s). If only one list is provided, it will be combined (pairwise) with itself.
#' @param by Function to apply. Default: c("intersect", "union", "c")
#' @return List with combined elements
#' @seealso 
#'  \code{\link[tidyr]{expand_grid}}
#'  \code{\link[stats]{setNames}}
#' @rdname pairup
#' @export 
#' @importFrom tidyr expand_grid
#' @importFrom stats setNames
pairup = function(..., by = c('intersect','union','c')) {
    by = switch(match.arg(by), union = union, intersect = intersect, c = c)
    dots = sapply(list(...), as.list, simplify = F)
    if (length(dots) == 1) dots[[2]] <- dots[[1]]
    #nams = paste('V', pretty_numbers(1:length(dots)), sep = '')
    names(dots) = 1:length(dots)
    resdat = as.data.frame(t(do.call(tidyr::expand_grid, dots)))
    reslist = sapply(resdat, function(v) Reduce(by, v), simplify = F)
    rm(resdat)
    namdat = as.data.frame(t(do.call(tidyr::expand_grid, sapply(dots, names, simplify = F))))
    namvec = unlist(sapply(namdat, function(v) do.call(paste0, list(v, collapse = '.')), simplify = F))
    rm(namdat)
    stats::setNames(reslist, namvec)
}
