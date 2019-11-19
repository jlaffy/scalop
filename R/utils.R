## approximate equality
about_equal = function(x,y,tol=1e-10) {
    stopifnot(is.numeric(x), is.numeric(y), length(x)==length(y), all(abs(x-y) < tol))
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

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
