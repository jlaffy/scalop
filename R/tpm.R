#' @title Convert TPM to logTPM 
#' @description Convert TPM to logTPM, i.e. using log2(TPM/10 + 1).
#' @param m matrix of logTPM values (gene rows; cell columns)
#' @param bulk if bulk then instead uses log2(TPM + 1). i.e. no scaling. Default: F
#' @return TPM matrix 
#' @details TPM/10 is used for single cells since 100,000 is a more reasonable estimate than 1,000,000 for the number of RNA transcripts in a cell. 1,000,000 is reasonable estimate for bulk samples that contain multiple cells.
#' @rdname unlogtpm
#' @export 
logtpm = function(m, bulk = F) {
    if (has_dim(m)) m = as.matrix(m)
    if (bulk) x = 1
    else x = 10
    log2((m/x) + 1)
}

#' @title Convert logTPM to TPM 
#' @description Convert logTPM to TPM, i.e. using 10*(2^(TPM)-1).
#' @param m matrix of logTPM values (gene rows; cell columns)
#' @param bulk if bulk then instead uses 2^(TPM)-1. i.e. no scaling. Default: F
#' @return TPM matrix 
#' @details TPM/10 is used for single cells since 100,000 is a more reasonable estimate than 1,000,000 for the number of RNA transcripts in a cell. 1,000,000 is reasonable estimate for bulk samples that contain multiple cells.
#' @rdname unlogtpm
#' @export 
unlogtpm = function(m, bulk = F) {
    # wrapper around scalop::tpm since scalop::tpm is confusing..
    # in that it does not generate tpm from counts, but rather removes log, scaling and pseudocount
    if (has_dim(m)) m = as.matrix(m)
    if (bulk) x = 1
    else x = 10
    x * (2^(m) - 1)
}

#' @title Convert logTPM to TPM 
#' @description 'This function has been deprecated. Please instead use `scalop::unlogtpm`'
#' @rdname tpm
#' @export
tpm = function(m, bulk = F) {
    message('This function has been deprecated. Please instead use `scalop::unlogtpm`')
}
