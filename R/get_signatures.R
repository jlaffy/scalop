
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @param signatures PARAM_DESCRIPTION, Default: scalop::NormalSignatures
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname get_signatures
#' @export 
#' @importFrom stringr str_detect
get_signatures = function(pattern, signatures = NormalSignatures) {
    signatures[stringr::str_detect(names(signatures), pattern)]
}
