
dea_order = function(dea.l) {
    nsig1 = sapply(dea.l, function(df) sum(df$p.adj <= 0.01, na.rm = TRUE))
    nsig2 = sapply(dea.l, function(df) sum(df$p.adj <= 0.001, na.rm = TRUE))
    order(nsig1, nsig2, decreasing = T)
}

dea_sort = function(dea.l, ...) {
    dots = list(...)
    ord = dea_order(dea.l)
    dea.l = dea.l[ord]
    if (length(dots) >= 1) {
        dots = sapply(dots, function(dot) dot[ord], simplify = F)
    }
    c(list(dea.l), dots)
}

