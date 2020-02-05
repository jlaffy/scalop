
#' @title Set the rownames in an object
#' @description This is a convenience function that sets the rownames on an object, in the same manner as stats::setNames
#' @param object an object for which a ‘rownames’ attribute will be meaningful. Default: nm
#' @param nm a character vector of rownames to assign to the object
#' @return An object of the same sort as ‘object’ with the new rownames
#' @rdname setRowNames
#' @export 
setRowNames = function(object = nm, nm) {
    rownames(object) <- nm
    object
}
