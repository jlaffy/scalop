
#' @rdname combine_group2_dea
#' @export
combine_group2_dea = function(L.dea, coverage = 0.5, return.avg.lfc = FALSE) {
    df = scalop::ldcast(L.dea)
    df = as.matrix(df)
    avg.dea = rowMeans(df, na.rm = T)
    coverage.dea = as.numeric(rowMeans(!is.na(df)) >= coverage)
    ord = order(coverage.dea, avg.dea, decreasing = TRUE)
    if (return.avg.lfc) {
        return(avg.dea[ord])
    }
    rownames(df)[ord]
}
