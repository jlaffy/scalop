
#' @title Character List to Binary (membership) Matrix 
#' @description Convert a list of character vectors to a matrix with i columns and j rows, where i are the list vector names and j are the unique elements found across i vectors. Cell i,j has a value of 1 if element j exists in vector i, and a value of 0 otherwise.
#' @param L list of character vectors
#' @return matrix of 0s and 1s with as many columns as there are vectors in the list and as many rows as there are unique elements across all vectors.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  L = list(orig = letters, subset = sample(letters, 5))
#'  head(mat01(L))
#'  }
#' }
#' @rdname mat01
#' @export 
mat01 = function(L) {
    items = unique(unlist(L))
    m = sapply(L, function(l) as.numeric(items %in% l))
    m = as.data.frame(m)
    rownames(m) = items
    m
}
