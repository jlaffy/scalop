
#' @title Convert sc-matrix to simulated bulk 
#' @param m non-row-centered logTPM or TPM matrix.
#' @param groups if not NULL, return a simulated bulk column per group in groups. Default: NULL
#' @param isLog set to FALSE if 'm' is in TPM form. Default: TRUE
#' @return simulated bulk matrix. If groups is NULL, one column is returned for the average across all cells. Else one column per group in 'groups'.
#' @rdname as_bulk
#' @export 
as_bulk = function(m, groups = NULL, isLog = TRUE, by = 'mean') {
    if (isLog) {
        m = unlogtpm(m, bulk = FALSE)
    }

    if (by == 'median') FUN = matrixStats::rowMedians
    else FUN = matrixStats::rowMeans2
    
    .as_bulk = function(m) logtpm(FUN(m), bulk = TRUE)
    
    if (is.null(groups)) {
        groups = colnames(m)
    }

    if (is.character(groups)) {
        groups = list(groups)
    }
    
    res = sapply(groups, function(group) .as_bulk(m[, group]))
    rownames(res) = rownames(m)
    res
}

#' @title Convert sc-matrix to simulated bulk samples matrix 
#' @param m non-row-centered logTPM or TPM matrix.
#' @param samples a list of cell IDs per sample. Default: split_by_sample_names(colnames(m))
#' @param isLog set to FALSE if 'm' is in TPM form. Default: TRUE
#' @return simulated bulk matrix,with as many columns as there are samples in the <samples> list.
#' @rdname as_bulk_samples
#' @export 
as_bulk_samples = function(m,
                           samples = split_by_sample_names(colnames(m)),
                           isLog = TRUE, 
                           by = 'median') {
    as_bulk(m = m, groups = samples, isLog = isLog, by = by)
}
