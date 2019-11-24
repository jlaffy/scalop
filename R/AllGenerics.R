## Generic functions for package scalop

#' @export
setGeneric("rowFtests", function(x, fac, var.equal=TRUE)
           standardGeneric("rowFtests"))

#' @export
setGeneric("colFtests", function(x, fac, var.equal=TRUE)
           standardGeneric("colFtests"))


#' @export
setGeneric("rowttests", function(x, fac, tstatOnly=FALSE, na.rm = FALSE)
           standardGeneric("rowttests"))


#' @export
setGeneric("colttests", function(x, fac, tstatOnly=FALSE, na.rm = FALSE)
           standardGeneric("colttests"))


#' @export
#' @rdname hca
setGeneric("hca", function(x, ...)
           standardGeneric("hca"))