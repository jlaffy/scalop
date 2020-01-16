
#' @title Convert a list to a jaccard matrix and plot with `scalop::gmap`
#' @param L list of character vectors to compute jaccard indexes between. 
#' @param ratio aspect ratio. Default: 1
#' @param limits colour key limits. Default: c(0, 1)
#' @param tile.col draw lines between tiles. Default: 'black'
#' @param tile.size draw lines between tiles. Default: 0.1
#' @param cols colours for key. Default: c("white", "darkred")
#' @param num numeric (TRUE) or character (FALSE) x- and y- axes labels. Default: FALSE
#' @param angle angle x-axis labels. Default: T
#' @param ... other arguments passed to `scalop::gmap` 
#' @param tiletext write jaccard indexes on tiles? Default: FALSE
#' @param dcp relevant if tiletext is TRUE. How many decimal places to show? Default: 1
#' @return ggplot object 
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{geom_label}}
#' @rdname jacmap
#' @export 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_text
jacmap = function(L = NULL,
                  L2 = NULL,
                  m = NULL,
                  ratio = 1,
                  limits = c(0, 1),
                  tile.col = 'black',
                  tile.size = 0.1,
                  cols = c('white', 'darkred'),
                  num = FALSE,
                  angle = T,
                  ...,
                  tiletext = FALSE,
                  dcp = 1) {

    if (is.null(m)) {
        m = hca_reorder(jaccard(x = L, y = L2))
    }

    d = reshape2::melt(m)

    G = gmap(d,
             ratio = ratio,
             limits = limits,
             tile.col = tile.col,
             tile.size = tile.size,
             cols = cols,
             num = num,
             angle = angle,
             ...)

    if (tiletext) {
        G = G + ggplot2::geom_text(aes(label = round(value, dcp)))
    }

    plot(G)
    invisible(list(G = G, d = d))
}
