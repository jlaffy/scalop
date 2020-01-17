
#' @title Join a dataframe and a named vector 
#' @description FUNCTION_DESCRIPTION
#' @param dat dataframe
#' @param vec named vector whose names are a column of the dataframe
#' @param by the name of the column that joining should be performed on.
#' @param newcol the name of the new column to add from vec. Default: 'val'
#' @return dataframe with the same number of rows as 'dat' and an additional column corresponding to the values of 'vec'.
#' @seealso 
#'  \code{\link[dplyr]{join}}
#' @rdname add_to_dataframe
#' @export 
#' @importFrom dplyr left_join
add_to_dataframe = function(dat, vec, by, newcol = 'val') {
    dat.vec = data.frame(join=names(vec), val=vec, stringsAsFactors = F)
    colnames(dat.vec) = c(by, newcol)
    dplyr::left_join(dat, dat.vec)
}
