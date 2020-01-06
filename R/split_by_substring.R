#' @title Split Character Vector by Pattern Matching
#' @description Split character vector into subsets according to detected pattern matches.
#' @param x character vector
#' @param pattern character vector with pattern(s) to match
#' @return list of character vectors with patterns as list names
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname split_by_substring
#' @export 
#' @importFrom stringr str_detect
split_by_substring = function(x, pattern) {
    stopifnot(is.character(x) & is.character(pattern))
    sapply(pattern, function(pat) {
               x[stringr::str_detect(x, pat)]},
               simplify = F)
}

