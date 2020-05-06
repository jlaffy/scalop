
#' @title A plot-friendly output for the Barnes-Hut implementation of t-SNE.
#' @description A plot-friendly output for the Barnes-Hut implementation of t-SNE: test different perplexities and dimensions. See Rtsne::Rtsne for details on the algorithm.
#' @param m matrix; each row is an observation (eg. cell); each column is a feature (eg. gene / principal component) 
#' @param perplexity numeric vector of perplexities to use. Default: c(15, 30, 50)
#' @param ndim numeric vector of dimensions to use. Default: ncol(m)
#' @param ... other arguments passed to Rtsne::Rtsne
#' @return a dataframe with columns: id, tSNE1, tSNE2, perplexity, ndim
#' @details Every combination of values for 'perplexity' and 'ndim' will be run if multiple are provided for each.
#' @seealso 
#'  \code{\link[Rtsne]{Rtsne}}
#'  \code{\link[dplyr]{join}}
#' @rdname ohmytsne
#' @export 
#' @importFrom Rtsne Rtsne
#' @importFrom dplyr full_join
ohmytsne = function(m,
                    perplexity = c(15, 30, 50),
                    ndim = ncol(m),
                    ...) {

    ndim = as.list(ndim) 
    perplexity = as.list(perplexity)
    out = data.frame(id = '',
                     tSNE1 = 0,
                     tSNE2 = 0,
                     perplexity = 0,
                     ndim = 0,
                     stringsAsFactors = FALSE)

    for (d in ndim) {

        mi = m[, 1:d]

        for (p in perplexity) {
            dat = as.data.frame(Rtsne::Rtsne(mi, perplexity = p, ...)$Y)
            colnames(dat) = c('tSNE1', 'tSNE2')
            dat$id = rownames(mi)
            dat$perplexity = rep(p, nrow(dat))
            dat$ndim = rep(d, nrow(dat))
            out = suppressMessages(dplyr::full_join(out, dat))
        }

    }

    out[-1, ]
}
