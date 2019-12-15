#' @title Get unique sample names
#' @description Extract unique sample names from cell ids
#' @param x character vector of cell ids
#' @param sep separator Default: '-|_'
#' @param pos position of sample name given separator. First position is 1. Default: 1
#' @param max.nchar maximum number of characters to take as sample name (starting from the first). Default: NULL
#' @return character vector of unique sample names
#' @rdname get_sample_names
#' @export 
#' @importFrom stringr str_split
get_sample_names = function(x, sep = "-|_", pos = 1, max.nchar = NULL) {

    samples = sapply(stringr::str_split(x, sep), `[`, 1)

    if (!is.null(max.nchar)) {
        samples = sapply(samples, function(sample) {
                             if (nchar(sample) <= max.nchar) sample
                             else substr(sample, 1, max.nchar)})
        samples = unique(samples)
    }

    if (length(unique(toupper(samples))) < length(samples)) {
        samples = toupper(samples)
        samples = unique(samples)
    }

    samples
}
