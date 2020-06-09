
#' @title Row Means of Log2 data 
#' @description calculate row means in normal space and then convert back to log2.
#' @param x numeric matrix
#' @param type how was the data normalised? "tpm"; Transcripts Per Million. "cpm"; Counts Per Million. Default: "tpm"
#' @return vector of log2 row means 
#' @rdname logRowMeans
#' @export 
logRowMeans = function(x, type = c('tpm', 'cpm')) {
    type = match.arg(type)
    if (type == 'cpm') {
        return(logcpm(rowMeans(unlogcpm(x))))
    }
    logtpm(rowMeans(unlogtpm(x)))
}

