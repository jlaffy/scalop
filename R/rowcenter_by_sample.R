
rowcenter_by_sample = function(m, samples = split_by_sample_names(colnames(m))) {
    res = sapply(samples, function(cells) rowcenter(m[, cells]), simplify = F)
    Names = unlist(sapply(res, colnames, simplify = F))
    res = do.call(cbind.data.frame, res)
    colnames(res) <- Names
    res
}
