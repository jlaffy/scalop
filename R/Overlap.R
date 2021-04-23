.overlap = function(x,y) {
	length(intersect(x,y))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION, Default: NULL
#' @param simplify PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname overlap
#' @export 
overlap = function (x, y = NULL, simplify = TRUE) {
    if (!is.null(dim(x))) {
        x = as.list(as.data.frame(x, stringsAsFactors = F))
    }
    if (is.null(y)) {
        y = x
    }
    if (!is.null(dim(y))) {
        y = as.list(as.data.frame(y, stringsAsFactors = F))
    }
    are.chars = sum(sapply(list(x, y), is.character)) == 2
    are.lists = sum(sapply(list(x, y), is.list)) == 2
    stopifnot(are.chars | are.lists)
    if (are.chars) {
        return(.jaccard(x = x, y = y))
    }
    comply(x, y, FUN = .overlap)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION, Default: NULL
#' @param cutoff PARAM_DESCRIPTION, Default: 0
#' @param row PARAM_DESCRIPTION, Default: T
#' @param col PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Overlap
#' @export 
Overlap = function(x, y = NULL, cutoff = 0, row = T, col = F) {
    ov = overlap(x, y)
    if (row) ov = ov[apply(ov, 1, function(row) any(row >= cutoff)),]
    if (col) ov = ov[, apply(ov, 2, function(row) any(row >= cutoff))]
    ov
}

