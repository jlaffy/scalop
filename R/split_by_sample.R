
#' @title Split Character Vector by Pattern Matching
#' @description Split character vector into subsets according to detected pattern matches.
#' @param x character vector
#' @param pattern character vector with pattern(s) to match
#' @return list of character vectors with patterns as list names
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname split_by_match
#' @export 
#' @importFrom stringr str_detect
split_by_match = function(x, pattern) {
    stopifnot(is.character(x) & is.character(pattern))
    sapply(pattern, function(pat) {
               x[stringr::str_detect(x, p)]},
               simplify = F)
}

#' @title Split Character Vector of Cell IDs by Sample Names
#' @description Split character vector of cell ids into subsets according to detected sample names.
#' @param x character vector of cell IDs
#' @param pattern a character vector of sample names, or NULL. If NULL, uses scalop::get_sample_names to find samples. 
#' @param ... other arguments passed to scalop::get_sample_names.
#' @return list of character vectors with samples as list names
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname split_by_sample
#' @export 
#' @importFrom stringr str_detect
split_by_sample = function(x, samples = NULL, ...) {
    if (is.null(samples)) samples = get_sample_names(x, ...)
    split_by_match(x, pattern = samples)
}
