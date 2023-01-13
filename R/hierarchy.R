
#' @title Cell state hierarchy coordinates
#' @description Cell state hierarchy coordinates
#' @param m matrix of cell state scores (cell rows; state columns)
#' @param quadrants option to find pmax of a subset of columns and use this as a state quadrant. pass argument in the form of a list of character vectors (column names), where list elements should start from bottom and go from left to right. Default: NULL
#' @param log.scale scale the hierarchy coordinates in log2 space. Default: T
#' @return dataframe with X and Y coordinate values.
#'  \code{\link[dplyr]{mutate}}
#' @rdname hierarchy
#' @export 
hierarchy = function(m, quadrants = NULL, log.scale = T) {

    if (!is.null(quadrants)) {
        stopifnot(all(unlist(quadrants) %in% colnames(m)))
        dat = as.data.frame(sapply(quadrants, function(col) do.call(pmax, list(as.data.frame(m[, col])))))
    } else {
        stopifnot(ncol(m) == 4)
        dat = as.data.frame(m)
    }

    rows = rownames(m)
    colnames(dat) = c('bl', 'br', 'tl', 'tr')

    dat = dplyr::mutate(dat,
                        bottom = pmax(bl, br),
                        top = pmax(tl, tr),
                        b.center = br - bl,
                        t.center = tr - tl,
                        x = ifelse(bottom > top, b.center, t.center), # dependent var
                        x.scaled = (sign(x) * log2(abs(x) + 1)),
                        y = top - bottom, # independent var
                        y.scaled = (sign(y) * log2(abs(y) + 1)))

    if (!log.scale) dat = dplyr::transmute(dat, X = x, Y = y)
    else dat = dplyr::transmute(dat, X = x.scaled, Y = y.scaled)
    rownames(dat) = rows
    dat
}
