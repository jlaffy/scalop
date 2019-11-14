
#' @title many-bar annotations ggplot
#' @description many-bar annotations ggplot
#' @param ... named character vectors of observations and groups (names). All vectors must contain the same observations, and the ordering in the first vector provided is preserved.
#' @param titles character vector of annomap titles. Default: NULL
#' @param flip observations along y axis instead of x? Default: F
#' @param ratio aspect ratio. If flip = T, ratio = 1/ratio.. Default: 0.03
#' @param mar see plot.margin in ggplot2::themes for details. Default: 0.015
#' @param angle logical indicating whether to angle title if flip = T. Default: T
#' @return ggplot object consisting of ggplots combined.
#' @seealso 
#'  \code{\link[ggplot2]{waiver}},\code{\link[ggplot2]{theme}}
#' @rdname annomaps
#' @export 
#' @importFrom ggplot2 waiver theme
annomaps = function(..., titles = NULL, flip = F, ratio = 0.03, mar = 0.015, angle = T) {
    .annomap = function(X, pal, title = NULL, breaks = NULL) {
        annomap(X, title = title, flip = flip,
                ratio = ratio, breaks = breaks,
                pal = pal, angle = angle, mar = mar)
    }
    dots = list(...)
    lev = levels(dots[[1]])
    if (is.null(lev)) lev = dots[[1]]
    dots = sapply(dots, function(d) factor(d, levels = lev), simplify = F)
    len = length(dots) 
    if (is.null(titles)) titles = rep('', len+1)
    firstmap = list(.annomap(dots[[1]], title = titles[[1]], pal = 1, breaks = ggplot2::waiver()))
    maps = sapply(2:len, function(i) .annomap(X = dots[[i]], pal = i, title = titles[[i]]), simplify = F)
    maps = c(firstmap, maps)
    if (flip) by = 'y'
    else by = 'x'
    do.call(grombine, c(... = maps, list(by = by)))
}
