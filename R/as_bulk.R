
#' @title Convert sc-matrix to simulated bulk 
#' @param m non-row-centered logTPM or TPM matrix.
#' @param groups if not NULL, return a simulated bulk column per group in groups. Default: NULL
#' @param isLog set to FALSE if 'm' is in TPM form. Default: TRUE
#' @return simulated bulk matrix. If groups is NULL, one column is returned for the average across all cells. Else one column per group in 'groups'.
#' @rdname as_bulk
#' @export 
as_bulk = function(m, groups = NULL, isLog = TRUE) {
    if (isLog) {
        m = tpm(m, bulk = FALSE)
    }

    .as_bulk = function(m) logtpm(rowMeans(m), bulk = TRUE)
    
    if (is.null(groups)) {
        groups = colnames(m)
    }

    if (is.character(groups)) {
        groups = list(groups)
    }
    
    sapply(groups, function(group) .as_bulk(m[, group]))
}
