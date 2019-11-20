tpm = function(m, bulk = F) {
    if (has_dim(m)) m = as.matrix(m)
    if (bulk) x = 1
    else x = 10
    x * (2^(m) - 1)
}
