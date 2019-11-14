
#' @title One-bar annotation ggplot
#' @description one-bar annotation ggplot
#' @param X named character vector of observations and groups (names); or a dataframe with first two columns as observations and groups.
#' @param title annomap title. Default: NULL
#' @param x.title x axis (observations) title (e.g. 'cells'). Default: NULL
#' @param pal numeric value specifying different qualitative brewer palettes. See ggplot2::scale_fill_brewer for details. Default: 1
#' @param angle logical indicating whether to angle title if flip = T. Default: T
#' @param breaks set to NULL to remove x axis tick marks. Default: ggplot2::waiver()
#' @param x.num enumerate observations on x axis? Default: T
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
#' @importFrom ggplot2 waiver ggplot scale_x_continuous scale_x_discrete geom_tile theme_bw scale_y_continuous theme element_text scale_fill_brewer scale_fill_manual coord_flip
#' @importFrom ggpubr get_legend rremove
annomap = function(X,
                   title = NULL,
                   x.title = NULL, 
                   pal = 1,
                   angle = F,
                   breaks = ggplot2::waiver(),
                   x.num = T,
                   flip = F,
                   hide.legend = T,
                   legend.pos = 'top',
                   ratio = 0.03,
                   mar = 0.015,
                   cols = NULL,
                   cols.order = names(cols)) {

    # fix dataframe
    if (!is.null(dim(X))) {
        X = as.data.frame(X)
        colnames(X) = c('x', 'fill')
    }
    # OR make dataframe
    else if (is.atomic(X)) {
        stopifnot(!is.null(names(X)))
        if (!is.null(levels(X))) X = X[match(X, levels(X))]
        X = data.frame(x = X, fill = names(X))
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

    # ggplot x scale
    if (x.num) {
        G = ggplot2::ggplot(X, aes(x = as.numeric(x), fill = fill, y = 1))
        scale_x_choose = ggplot2::scale_x_continuous
    }
    else {
        G = ggplot2::ggplot(X, aes(x = x, fill = fill, y = 1))
        scale_x_choose = ggplot2::scale_x_discrete
    }

    # plot
    G = G + ggplot2::geom_tile() + 
            ggplot2::theme_bw() +
            scale_x_choose(expand = expand,
                           position = x.title.pos,
                           breaks = breaks,
                           name = x.title) +
            ggplot2::scale_y_continuous(name = title,
                                        expand = expand,
                                        breaks = NULL,
                                        position = y.title.pos) +
            ggplot2::theme(aspect.ratio = ratio,
                           legend.position = legend.pos,
                           axis.title.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust), 
                           axis.title.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust),
                           plot.margin = ggplot2::margin(mar, mar, mar, mar, "cm")) + 
            ggplot2::scale_fill_brewer(palette = pal, type = 'qual')

    if (!is.null(cols.order)) {
        col.groups = unique(X$fill)
        if (is.null(cols)) cols = rainbow(n = length(col.groups))
        cols = cols[match(col.groups, cols.order)]
        G = G + ggplot2::scale_fill_manual(values = cols)
    }

    if (flip) G = G + ggplot2::coord_flip()
    legend = ggpubr::get_legend(G)
    if (hide.legend) G = G + ggpubr::rremove("legend")
#    plot(G)
#    invisible(list(plot = G, data = X, legend = legend))

    G
}

