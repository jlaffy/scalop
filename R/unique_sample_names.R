#' @title Get unique sample names
#' @description Extract unique sample names from cell ids
#' @param x character vector of cell ids
#' @param sep separator Default: '-|_'
#' @param pos position of sample name given separator. First position is 1. Default: 1
#' @param max.nchar maximum number of characters to take as sample name (starting from the first). Default: NULL
#' @param replace a list of character vectors to replace. Takes the form list(c(old, new), c(old, new)). Default: NULL
#' @return character vector of unique sample names
#' @rdname unique_sample_names
#' @export 
#' @importFrom stringr str_split
unique_sample_names = function(x, sep = "-|_", pos = 1, max.nchar = NULL, replace = NULL) {

    #samples = sapply(stringr::str_split(x, sep), `[`, pos)
    samples = stringr::str_split(x, sep)
    samples = sapply(samples, `[`, pos, simplify = F)
    sep = sapply(stringr::str_split(sep, "\\|"), `[`, 1)
    samples = sapply(samples, paste0, collapse = sep, simplify = F)
    samples = as.character(unlist(samples))

    if (!is.null(max.nchar)) {
        samples = sapply(samples, function(sample) {
                             if (nchar(sample) <= max.nchar) sample
                             else substr(sample, 1, max.nchar)})
    }

    samples = unique(samples)

    if (!is.null(replace)) {
        for (i in 1:length(replace)) {
            r.old = paste0(replace[[i]], collapse = '|')
            r.new = replace[[i]][[2]]
            samples = stringr::str_replace(samples, r.old, r.new)
        }
    }

    unique(samples)
}
