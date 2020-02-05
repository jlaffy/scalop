
top = function(x, ...) {
    UseMethod('top', x)
}

top.default = function(...) NULL
top.data.frame = function(x, n = 0.1, index = FALSE, decreasing = TRUE) {
    x = as.data.frame(x)
    if (n <= 1 && n >= 0) n = n * nrow(x)
    ords = sapply(x, function(col) order(col, decreasing = decreasing)[1:n], simplify = F)
    if (index) return(ords)
    sapply(ords, function(ord) colMeans(x[ord,]), simplify = F)
}


top.matrix = top.data.frame
top.numeric = function(x, n = 0.1, index = FALSE, decreasing = T) {
    if (n <= 1 && n >= 0) n = n * length(x)
    ord = order(x, decreasing = decreasing)[1:n]
    if (index) return(ord)
    names(x)[ord]
}

top.character = top.numeric


program_variability = function(values, n = 0.1) {
    UseMethod('program_variability', values)
}

#program_variability.numeric = function(values, n = 0.1) {
#
#}

program_variability.data.frame = function(values, values2 = values, n = 0.1) {
    if (is.matrix(values)) values = as.data.frame(values)
    mapply(function(x, y = x, top, bottom) mean(x[top]) - mean(y[bottom]),
           x = sapply(as.list(values), setNames, rownames(values), simplify = F),
           y = sapply(as.list(values2), setNames, rownames(values2), simplify = F),
           top = top(values, n = n, decreasing = T),
           bottom = top(values2, n = n, decreasing = F))
}

program_variability.matrix = program_variability.data.frame
