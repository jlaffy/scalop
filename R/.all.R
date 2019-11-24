
genecount <-  function(p = 0.05,
                        fc = 3L,
                        n = 50,
                        return.data = F) {
    function(x) {
      stopifnot(is.data.frame(x))
      stopifnot(all(c('gene', 'foldchange', 'p.value') %in% colnames(x)))
      if (is.null(fc)) fc = -Inf
      if (is.null(p)) p = Inf
      x = dplyr::filter(x, foldchange >= fc, p.value <= p)
      if (return.data) return(x)
      if (is.null(n)) return(nrow(x))
      n >= nrow(x)
  }
}

nsig1 = function(p = 0.05, fc = 3L, n = 50) {
    genecount(p = p, fc = fc, n = n)
}

nsig2 = function(p = 0.005, fc = 3L, n = 10) {
    genecount(p = p, fc = fc, n = n)
}


signature_order = function(x, ...) {
    flist = list(...)
    sapply(flist, sapply, x, simplify = F)
}

signature_filter = function() {

}


genefilter <- function(expr, flist)
    {
     if(is(expr, "ExpressionSet"))
       expr <- exprs(expr)
     apply(expr, 1, flist)
}

filterfun <- function(...) {
     flist <- list(...)
 #let the user supply a list
     if( length(flist) == 1 && is.list(flist[[1]]) )
         flist <- flist[[1]]
     f <- function( x ) {
         for( fun in flist ) {
             fval <- fun(x)
             if( is.na(fval) || ! fval )
                 return(FALSE)
         }
             return(TRUE)
     }
     class(f) <- "filterfun"
     return(f)
 }
