#' @title Split Character Vector of Cell IDs by Sample Names
#' @description Split character vector of cell ids into subsets according to detected sample names.
#' @param x character vector of cell IDs
#' @param samples a character vector of sample names, or NULL. If NULL, uses scalop::unique_sample_names to find samples. 
#' @param sep ignored if <samples> is provided. separator Default: '-|_'
#' @param pos ignored if <samples> is provided. position of sample name given separator. First position is 1. Default: 1
#' @param max.nchar ignored if <samples> is provided. maximum number of characters to take as sample name (starting from the first). Default: NULL
#' @param replace ignored if <samples> is provided. a list of character vectors to replace. Takes the form list(c(old, new), c(old, new)). Default: NULL
#' @return list of character vectors with samples as list names
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname split_by_sample_names
#' @export 
#' @importFrom stringr str_detect
split_by_sample_names = function(x,
                                 samples = NULL,
                                 sep = "-|_",
                                 pos = 1,
                                 max.nchar = NULL,
                                 replace = NULL) {

    if (is.null(samples)) samples = unique_sample_names(x,
                                                        sep = sep,
                                                        pos = pos,
                                                        max.nchar = max.nchar,
                                                        replace = replace)
    split_by_substring(x, pattern = samples)
}

