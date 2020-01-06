#' @title Extract Sample Names 
#' @description Convenient wrapper function for `scalop::extract_substring`. Extract sample names from character vector. Sample names can be provided or, if not, then are found with `scalop::unique_sample_names`. 
#' @param x character vector of strings to extract sample names from
#' @param ... other arguments passed to `scalop::unique_sample_names`. Relevant only if sample_names is not provided.
#' @param sample_names character vector of unique sample names to extract from x. Default: NULL
#' @return character vector of sample names of the same length as x.
#' @rdname extract_sample_names
#' @export 
extract_sample_names = function(x, ..., sample_names = NULL) {
    if (is.null(samples)) sample_names = unique_sample_names(x, ...)
    extract_substring(x = x, choose = sample_names)
}
