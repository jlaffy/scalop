
#' @title Plot State Hierarchy (Quadrant plot)
#' @description Plot state hierarchy
#' @param X two-column matrix or dataframe with x and y coordinates.
#' @param quadrant.names character vector to label quadrants. Default: c("bl", "br", "tl", "tr")
#' @param main plot title. Default: NULL
#' @param xlab x-axis title. Default: 'Relative meta-module score [log2(|SC1-SC2|+1)]'
#' @param ylab y-axis title. Default: 'Relative meta-module score [log2(|SC1-SC2|+1)]'
#' @param groups a named character vector, where the vector corresponds to rownames in X and names are the groups to colour by. Default: NULL
#' @param group.cols named character vector of colours to use for each group. Names should match names in groups. Default: NULL
#' @param legend logical to draw legend if groups are provided. Default: T
#' @param legend.pos legend position. Default: 'bottom'
#' @param legend.horiz logical indicating whether legend should be horizontally drawn. Default: T
#' @return base plot
#' @rdname plot_hierarchy
#' @export 
plot_hierarchy = function(X,
                          quadrant.names = c('bl', 'br', 'tl', 'tr'),
                          main = NULL,
                          xlab = 'Relative meta-module score [log2(|SC1-SC2|+1)]',
                          ylab = 'Relative meta-module score [log2(|SC1-SC2|+1)]',
                          groups = NULL,
                          group.cols = NULL, 
                          legend = T,
                          legend.pos = 'bottom',
                          legend.horiz = T) {

    if (is.null(groups)) col = 'darkred'
    else col = 'grey85'

    graphics::plot(X[,1], X[,2], pch = 20, col = col, main = main, xlab = xlab, ylab = ylab)

    if (is.null(groups)) legend = F
    else {
        stopifnot(!is.null(names(groups)))
        stopifnot(all(groups %in% rownames(X)))
        groups = split(groups, names(groups))
        Xgrp = sapply(groups, function(rows) X[rows,,drop = F], simplify = F)
        if (!is.null(group.cols)) colgrp = group.cols[names(groups)]
        else colgrp = grDevices::rainbow(n = length(Xgrp))
        Map(graphics::points,
            x = sapply(Xgrp, `[[`, 1, simplify = F),
            y = sapply(Xgrp, `[[`, 2, simplify = F),
            col = colgrp,
            MoreArgs = list(pch = 20))
    }

    graphics::abline(v = 0, lty = 2)
    graphics::abline(h = 0, lty = 2)

    if (legend) {
        legend(legend.pos,
               fill = colgrp,
               legend = names(groups),
               horiz = legend.horiz,
               cex = 0.8,
               box.col = 'white',
               bg = 'white',
               box.lwd = 0)
    }

    Names = quadrant.names
    cex = 1.2
    graphics::mtext(side = 1, adj = 0, text = Names[1], cex = cex, line = cex - 1)
    graphics::mtext(side = 1, adj = 1, text = Names[2], cex = cex, line = cex - 1)
    graphics::mtext(side = 3, adj = 0, text = Names[3], cex = cex)
    graphics::mtext(side = 3, adj = 1, text = Names[4], cex = cex)
}
