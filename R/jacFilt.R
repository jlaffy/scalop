
#' @title Filter List By Jaccard  
#' @description ********* 
#' @param x PARAM_DESCRIPTION
#' @param threshold PARAM_DESCRIPTION, Default: 0.7
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname jacFilt
#' @export 
jacFilt = function(x, threshold = 0.7, which = FALSE) {

    .jacFilt = function(x, i, threshold) {
        vals = as.vector(jaccard(x[i], x), mode = 'numeric')
        vals[i] <- 0
        bool = vals < threshold
        x[bool]
    }

    if (which) {
        names(x) = 1:length(x)
    }

    i = 1
    while (i < length(x)) {
        x = .jacFilt(x = x, i = i, threshold = threshold)
        i = i + 1
    } 

    if (which) {
        x = as.numeric(names(x))
    }

    x
}
