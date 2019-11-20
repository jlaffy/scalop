logtpm = function(m, bulk = F) {
    if (has_dim(m)) m = as.matrix(m)
    if (bulk) x = 1
    else x = 10
    log2((m/x) + 1)
}
