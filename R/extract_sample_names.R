#' @title Extract Sample Names 
#' @description Convenient wrapper function for `scalop::extract_substring`. Extract sample names from character vector. Sample names can be provided or, if not, then are found with `scalop::unique_sample_names`. 
#' @param x character vector of strings to extract sample names from.
#' @param samples character vector of unique sample names to extract from x. Default: NULL
#' @param sep separator. Relevant only if samples = NULL. Default: "-|_|\\."
#' @param pos position of sample name given separator. First position is 1. Relevant only if samples = NULL. Default: 1
#' @param max.nchar maximum number of characters to take as sample name (starting from the first). Relevant only if samples = NULL. Default: NULL
#' @param replace a list of character vectors to replace. Takes the form list(c(old, new), c(old, new)). Relevant only if samples = NULL. Default: NULL
#' @return character vector of sample names of the same length as x.
#' @rdname extract_sample_names
#' @export 
extract_sample_names = function(x, 
                                samples = NULL, 
                                sep = "-|_",
                                pos = 1,
                                max.nchar = NULL,
                                replace = NULL) {
    if (is.null(samples)) {
        samples = unique_sample_names(x,
                                      sep = sep,
                                      pos = pos,
                                      max.nchar = max.nchar,
                                      replace = replace)
    }

    extract_substring(x = x, choose = samples)
}
