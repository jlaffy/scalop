#' @title Extract substrings between delimiters by position 
#' @description substri() splits each element in 'x' into substrings and returns the desired substring(s). 'sep' defines the delimiters separating the strings into substrings, such that the input data between the matches become the fields themselves. 'pos' defines the substring index(es) to be returned.
#' @param x A character vector.
#' @param sep The pattern to split each element of 'x' by. Default: '\\.|-|_'
#' @param pos The substring index(es) to be returned. Default: 1
#' @param max.nchar 'NULL', or a maximum number of characters that the substrings should be trimmed to contain. Default: NULL
#' @param na.rm Remove NA substring positions. Default: TRUE 
#' @return A character vector containing the extracted substrings. 
#' @examples
#' \dontrun{
#' x = c('dog.cat.mat', 'dog_frog')
#' substri(x)
#' substri(x, pos = 2)
#' substri(x, pos = c(1,3))
#' substri(x, pos = c(1,3), na.rm = FALSE)
#' substri(x, sep = "-")
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}},\code{\link[stringr]{str_replace_all}}
#' @rdname substri
#' @export 
#' @importFrom stringr str_split str_replace_all 
substri = function(x,
                   sep = "\\.|-|_",
                   pos = 1,
                   max.nchar = NULL,
                   na.rm = TRUE) {

    # separate strings into substrings
    strings = stringr::str_split(x, sep)
    # select relevant substrings
    strings = sapply(strings, `[`, pos, simplify = F)
    if (na.rm) {
        # remove NA positions
        strings = sapply(strings, function(s) s[!is.na(s)], simplify = F)
    }

    # if there is more than one substring,
    # paste selected substrings back together by the first sep delimiter
    sep = sapply(stringr::str_split(sep, "\\|"), `[`, 1)
    sep = stringr::str_replace_all(sep, "\\\\", "")
    strings = sapply(strings, paste0, collapse = sep, simplify = F)
    # convert strings from list to character vector
    strings = as.character(unlist(strings))
    # trim to keep 'max.nchar' characters per substring
    if (!is.null(max.nchar)) {
        strings = sapply(strings, function(string) {
            if (nchar(string) <= max.nchar)
                string
            else substr(string, 1, max.nchar)
        })
    }
    
    strings
}
