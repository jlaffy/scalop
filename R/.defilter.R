is_dea = function(x) {
    inherits(x, "dea")
}

as_dea = function(x) {
    if (is_dea(x)) return(x)
    if (!is.data.frame(x)) stop()
    if (!all(c('gene', 'foldchange', 'p.value') %in% colnames(x))) stop()
    class(x) = append(class(x), "dea")
    x
}

.filterSigDE = function(x, p, fc) {
    stopifnot(is_dea(x))
    if (is.null(fc)) fc = -Inf
    else fc = log2(fc)
    if (is.null(p)) p = Inf
    dplyr::filter(x, foldchange >= fc, p.value <= p)
}

nSigDE = function(p, fc) {
    function(x) {
        x = .filterSigDE(x, p = p, fc = fc)
        nrow(x)
    }
}

isSigDE = function(p, fc, N) {
    function(x) {
        x = .filterSigDE(x, p = p, fc = fc)
        nrow(x) >= N
  }
}

reducefun = function(..., rule = `&`) {
    flist = list(...)
    function(x) {
        Reduce(rule, sapply(flist, function(f) f(x), simplify = F))
    }
}

orderfun = function(..., decreasing = T, sort = F) {
    flist = list(...)
    function(x) {
        Vectors = sapply(flist, function(f) f(x), simplify = F)
        Args = c(...=Vectors, list(decreasing = decreasing))
        ord = do.call(order, Args, envir = environment())
        if (sort) return(x[ord])
    }
}

DEApply = function(filter = T, order = T) {

}

DEA = function() {

}


p1 = 0.05
p2 = 0.005
fc = 3L
N1 = 50
N2 = 10
DEorder = orderfun(nSigDE(p = p1, fc = fc), nSigDE(p = p2, fc = fc), decreasing = T)
DEsort = orderfun(nSigDE(p = p1, fc = fc), nSigDE(p = p2, fc = fc), decreasing = T, sort = T)
DEtest = reducefun(isSigDE(p = p1, fc = fc, N = N1), isSigDE(p = p2, fc = fc, N = N2), rule = `&`) 


is_sig = function(x,
                  m = NULL,
                  p.adjust.method = 'BH',
                  ngenes = NULL,
                  fc = NULL,
                  p = NULL,
                  flist = list(sigDE,
                               sigDE(p = 0.005, N = 10, fc = 2L)),
                  flist.rule = c('and', 'or')) {

    flist.rule = match.arg(flist.rule)
    flist.rule = switch(flist.rule, and = `&`, or = `|`)

    if (!is_dea(x)) {

        if (is.null(m)) {
            x = try(as_dea(x))
            if (class(x) == 'try-error') stop()
        }
        
        else {
            x = dea(groups = x,
                    m = m,
                    p = NULL,
                    fc = NULL,
                    p.adjust.method = p.adjust.method,
                    val = NULL)
        }
    }

    if (!is.null(ngenes) && (!is.null(fc) | !is.null(p))) {
        flist = list(sigfun(p = p, N = ngenes, fc = fc))
    }

    f = reducefun(unlist(flist), rule = flist.rule)
    f(x)
}
