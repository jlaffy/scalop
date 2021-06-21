
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#' @rdname ggraster
#' @export 
#' @importFrom reshape2 melt
ggraster = function(m, ...) {
    graster(reshape2::melt(as.matrix(m)), ...) 
}
