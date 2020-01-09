
dea_order = function(dea.l, ...) {
    FUNS = list(...)
    nsig1 = sapply(dea.l, function(df) sum(df$p.adj <= 0.01, na.rm = TRUE))
    nsig2 = sapply(dea.l, function(df) sum(df$p.adj <= 0.001, na.rm = TRUE))
    dea.l = dea.l[order(nsig1, nsig2, decreasing = T)]
}
