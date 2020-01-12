
#' @title One-bar annotation ggplot
#' @description one-bar annotation ggplot
#' @param X named character vector of observations and groups (names); or a dataframe with first two columns as observations and groups.
#' @param title annomap title. Default: NULL
#' @param x.title x axis (observations) title (e.g. 'cells'). Default: NULL
#' @param pal numeric value specifying different qualitative brewer palettes. See ggplot2::scale_fill_brewer for details. Default: 1
#' @param angle logical indicating whether to angle title if flip = T. Default: T
#' @param breaks set to NULL to remove x axis tick marks. Default: ggplot2::waiver()
#' @param num enumerate observations on x axis? Default: T
#' @param flip observations along y axis instead of x? Default: F
#' @param hide.legend logical. Default: T
#' @param legend.pos character string. See legend.position in ggplot2::theme for details. Default: 'top'
#' @param ratio aspect ratio. If flip = T, ratio = 1/ratio.. Default: 0.03
#' @param mar see plot.margin in ggplot2::themes for details. Default: 0.015
#' @param cols custom colour palette. Option to name colours by groups to choose individual group colours. Default: NULL
#' @param cols.order character vector with groups. Changing the order changes the assignment of colours to groups. Default: names(cols)
#' @return ggplot
#' @seealso 
#'  \code{\link[ggplot2]{waiver}},\code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{scale_x_discrete}},\code{\link[ggplot2]{geom_raster}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{scale_colour_brewer}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{coord_flip}}
#'  \code{\link[ggpubr]{get_legend}},\code{\link[ggpubr]{rremove}}
#' @rdname annomap
#' @export 
annomap = function(X,
                   y.name = NULL,
                   x.name = NULL, 
                   angle = F,
                   num = T,
                   flip = F,
                   hide.legend = T,
                   get.legend = F,
                   legend.pos = 'top',
                   ratio = 0.03,
                   tile.col = NULL,
                   tile.size = 0.01,
                   mar = 0.015,
                   cols = NULL,
                   limits = NULL,
                   ncol = 1,
                   patchwork = T,
                   widths = c(9, 1),
                   guides = 'collect',
                   ...) {

    if (is.character(X)) {
        X = data.frame(fill = X)
        rownames(X) = 1:nrow(X)
    }
    
    else if (is.list(X) & !is.data.frame(X)) {
        stopifnot(identical(lengths(X)))
        X = do.call(cbind.data.frame, X)
        rownames(X) = 1:nrow(X)
    } 

    # fix dataframe
    if ('x' != colnames(X)[1]) {
        X = X %>% tibble::rownames_to_column('x')
    }
    
    n = ncol(X)
    if (is.null(levels(X$x))) {
        X = dplyr::mutate(X, x = factor(as.character(x),levels = unique(as.character(x))))
    }

    # internal plotting vars
    if (angle == F) angle = 0
    expand = c(0, 0)
    y.title.pos = 'left'
    x.title.pos = 'top'
    hjust = 0.5
    vjust = 0.5 
    # adjust for flipped plot
    if (flip) {
        ratio = 1/ratio
        y.title.pos = 'right'
        x.title.pos = 'bottom'
        if (angle == T) {
            angle = 45
            hjust = 0.5
            vjust = 0.5
        }
    }
    
    cols = colnames(X)
    Gs = sapply(2:n, function(i) 
                {
                    X = X[, c(1, i)]
                    colnames(X) = c('x', 'fill')

                    if (!is.null(tile.col) || !is.null(tile.size)) {
                        geomTile = ggplot2::geom_tile(col = tile.col, size = tile.size)
                    } else {
                        geomTile = ggplot2::geom_tile()
                    }
                    if (flip) {
                        G = ggplot2::ggplot(X, aes(y = as.numeric(x), fill = fill, x = 1)) +
                            geomTile +
                            ggplot2::scale_y_discrete(expand = c(0,0)) +
                            ggplot2::scale_x_continuous(expand = c(0,0), breaks = NULL) +
                            ggplot2::coord_flip()
                    }
        
                    else {
                        G = ggplot2::ggplot(X, aes(x = as.numeric(x), fill = fill, y = 1)) +
                            geomTile +
                            ggplot2::scale_x_discrete(expand = c(0,0)) +
                            ggplot2::scale_y_continuous(expand = c(0,0), breaks = NULL)
                    }
        
                    if (is.numeric(X$fill)) {
                        if (is.null(limits)) limits = range(X$fill)
                        G = G + ggplot2::scale_fill_gradientn(colours = brewerland::colourPal('Blues'), oob = scales::squish, limits = limits)
                    }
        
                    else {
                        print(head(X$fill))
                        X = X %>% dplyr::mutate(fill = factor(as.character(fill), levels = unique(as.character(fill))))
                        G = G + ggplot2::scale_fill_manual(values = sample(brewerland::discrete_colours))
                    }
        
                    G = G + ggplot2::theme(aspect.ratio = ratio,
                                           legend.position = legend.pos,
                                           axis.title.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust), 
                                           axis.title.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust),
                                           plot.margin = ggplot2::margin(mar, mar, mar, mar, "cm")) +
                            ggplot2::labs(x = x.name, y = cols[i])

                    G

                }, simplify = F)

    if (!patchwork) return(Gs)
    patchwork::wrap_plots(Gs, guides = guides, ncol = ncol, widths = widths, ...)
}
