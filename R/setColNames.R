
#' @title Set the colnames in an object
#' @description This is a convenience function that sets the colnames on an object, in the same manner as stats::setNames
#' @param object an object for which a ‘colnames’ attribute will be meaningful. Default: nm
#' @param nm a character vector of colnames to assign to the object
#' @return An object of the same sort as ‘object’ with the new colnames
#' @rdname setColNames
#' @export 
setColNames = function(object = nm, nm) {
    colnames(object) <- nm
    object
}
