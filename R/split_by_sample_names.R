#' @title Split Character Vector of Cell IDs by Sample Names
#' @description Split character vector of cell ids into subsets according to detected sample names.
#' @param x character vector of cell IDs
#' @param samples a character vector of sample names, or NULL. If NULL, uses scalop::unique_sample_names to find samples. 
#' @param ... other arguments passed to scalop::unique_sample_names.
#' @return list of character vectors with samples as list names
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname split_by_sample_names
#' @export 
#' @importFrom stringr str_detect
split_by_sample_names = function(x, samples = NULL, ...) {
    if (is.null(samples)) samples = unique_sample_names(x, ...)
    split_by_substring(x, pattern = samples)
}

