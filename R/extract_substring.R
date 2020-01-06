#' @title Find and Extract substrings
#' @description Detect substring and extract if present for each string in a character vector.
#' @param x character vector of strings to extract substrings from.
#' @param choose character vector of unique substrings of which one is expected to match a given string in the character vector x. Default: NULL
#' @return character vector of detected substrings of the same length as x.
#' @details if there is more than one match, the longest substring is taken. i.e. if HEL and HELLO are in substring vector choose, HELLO will be returned. 
#' @seealso 
#'  \code{\link[stringr]{str_extract}}
#' @rdname extract_substring 
#' @export 
#' @importFrom stringr str_extract
extract_substring = function(x, choose) {
    choose = choose[order(nchar(choose), decreasing = T)]
    choose = paste0(choose, collapse = "|")
    stringr::str_extract(x, choose)
}

