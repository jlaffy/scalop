#' @title Split a vector, list, matrix or dataframe into  into a list of matrices or nested list according to substring matches.
#' @description subsplit() divides the data in 'x' into groups defined by the presence of shared substrings. If 'x' is a list, the substrings are derived from 'names(x)'. If 'x' is a matrix or data frame, the substrings are instead derived from 'colnames(x)' or, if 'by.row=FALSE', from 'rownames(x)'.
#' @param x A character vector, a named list, a matrix or data frame.
#' @param pattern The substrings to split by. If 'NULL', substrings will instead be defined from the delimiter 'sep' and position index 'pos' with an internal call to scalop::substri(). Default: NULL
#' @param by.row If 'TRUE' and 'x' is a matrix or data frame, the substrings are derived from 'rownames(x)' instead of 'colnames(x)'. Default: FALSE
#' @param sep The pattern to split each element of 'x' by. Default: '\\.|-|_'
#' @param pos The substring index(es) to be returned. Default: 1
#' @param max.nchar 'NULL', or a maximum number of characters that the substrings should be trimmed to contain. Default: NULL
#' @param replace A character vector containing two elements. The first is the pattern to be replaced and the second is the pattern that it will be replaced with. Alternatively, a list of such character vectors, or 'NULL' if no replacement is desired. Default: NULL
#' @param na.rm Remove NA substring positions. Default: TRUE
#' @return A list of the same type as 'x' and of length equal to the number of unique substrings found.
#' @examples 
#' \dontrun{
#' Names = c(rep('dog.cat', 5), rep('dog', 2), rep('cat', 2), 'frog')
#' x.vec = replicate(expr = sample(100, 5), n = 10, simplify = F)
#' names(x.vec) = Names
#' subsplit(x.vec, pattern = c('dog', 'cat', 'frog'))
#' x.mat = replicate(expr = sample(100, 5), n = 10, simplify = T);
#' colnames(x.mat) = Names
#' subsplit(x.mat)
#' subsplit(x.mat, by.row = T)
#' }
#' @seealso 
#'  \code{\link[stringr]{str_subset}}
#' @rdname subsplit
#' @export 
#' @importFrom stringr str_subset
subsplit = function(x,
                    by.row = FALSE,
                    sep = "\\.|-|_", 
                    pattern = NULL,
                    pos = 1,
                    max.nchar = NULL, 
                    replace = NULL,
                    na.rm = TRUE) {

    m = NULL
    L = NULL

    if (has_dim(x) | stringr::str_detect(class(x), "Matrix")) {
        m = x
        if (by.row) x = rownames(m)
        else x = colnames(m)
    }

    else if (is.list(x)) {
        L = x
        x = names(L)
    }

    if (!is.null(pattern)) {
        groupvec = unlist(sapply(pattern, function(pat) stringr:str_extract(x, pat), simplify = F))
    } else {
        groupvec = substri(x, sep = sep, pos = pos, max.nchar = max.nchar, na.rm = na.rm)
    }
    
    if (!is.null(replace)) {
        if (!is.list(replace)) {
            replace = list(replace)
        }
        for (i in 1:length(replace)) {
            r.old = paste0(replace[[i]], collapse = "|")
            r.new = replace[[i]][[2]]
            groupvec = stringr::str_replace(groupvec, r.old, r.new)
        }
    }

    groups = split(x, groupvec)

    if (!is.null(m) & by.row) return(sapply(groups, function(group) m[group,,drop=F], simplify = F))
    else if (!is.null(m)) return(sapply(groups, function(group) m[,group,drop=F], simplify = F))
    if (!is.null(L)) return(sapply(groups, function(group) L[group], simplify = F))

    groups
}
