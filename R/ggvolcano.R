
#' @title Volcano plot 
#' @description Perform differential expression and create a volcano plot.
#' @param m A matrix of features (e.g. genes) by observations (e.g. cells). Default: NULL
#' @param group The group (of observations) to test. This should be a character vector or a two-level factor in which the first level corresponds to the group. Default: NULL
#' @param group2 A second group to test against. This should be a character vector. If 'group2=NULL', defaults to all remaining observations in the matrix 'm'. Default: NULL
#' @param dea.res The output of scalop::dea(..., return.val = 'df'). If provided, the function will not perform DEA and skip straight to plotting. Default: NULL
#' @param pmethod The correction method for multiple testing, or 'none' if no correction is desired. See stats::p.adjust.methods for options. Default: 'BH'
#' @param alternative The alternative hypothesis. If 'alternative=NULL' and 'group2' was provided, defaults to 'two-sided'. Else defaults to 'greater'. Default: NULL
#' @param logp The minimum -log10(P) value at which differential expression is considered significant. Default: 2
#' @param lfc The minimum log2 foldchange value at which differential expression is considered signficant. Default: 1
#' @param n The number of top features to display. Default: 10
#' @param selected  Default: NULL
#' @param add.to.selected PARAM_DESCRIPTION, Default: NULL
#' @param selected.col PARAM_DESCRIPTION, Default: 'red'
#' @param xlab X-axis title. Default: 'Log2 Fold Change'
#' @param ylab Y-axis title. Default: '-Log10 P-value (adjusted)'
#' @param symmetric PARAM_DESCRIPTION, Default: TRUE
#' @param xintercept One or more numeric values to be added as vertical delimiters to the plot. Default: 0
#' @param ymax Y-axis upper limit. If 'NULL', defaults to the maximum -log10(P) value. Setting 'ymax' can be useful to 'zoom in' on the y-axis, making feature points/labels below 'ymax' easier to see. Default: NULL
#' @param xlim PARAM_DESCRIPTION, Default: NULL
#' @param text.size Axes text size. Default: 14
#' @param label.size Feature label text sizes. Default: 5
#' @param legend.text.size Legend text size. Default: 12
#' @return A list containing:
#' \itemize{
#' \item {data:} {A data frame; the differential expression results used in the plotting.}
#' \item {G:} {A ggplot2 object; the volcano plot.}
#' }
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{pull}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{geom_abline}}
#'  \code{\link[ggrepel]{geom_text_repel}}
#' @rdname ggvolcano
#' @export 
#' @importFrom dplyr mutate filter arrange pull
#' @importFrom ggplot2 ggplot geom_point scale_x_continuous scale_y_continuous theme_bw theme labs scale_colour_manual scale_size_manual geom_vline
#' @importFrom ggrepel geom_text_repel
ggvolcano = function(
                     m = NULL,
                     group = NULL,
                     group2 = NULL,
                     dea.res = NULL,
                     pmethod = 'BH',
                     alternative = NULL,
                     logp = 2,
                     lfc = 1,
                     n = 10,
                     selected = NULL,
                     add.to.selected = NULL,
                     selected.col = 'red',
                     xlab = 'Log2 Fold Change',
                     ylab = '-Log10 P-value (adjusted)',
                     symmetric = TRUE,
                     xintercept = 0, 
                     segment.colour = 'grey30',
                     text.size = 14,
                     label.size = 5,
                     legend.text.size = 12,
                     ymax = NULL,
                     xlim = NULL) {

    if (is.null(dea.res)) {

        if (is.null(alternative)) {
            if (is.null(group2)) alternative = 'greater'
            else alternative = 'two-sided'
        }

        dea.res = dea(m,
                      group = group,
                      group2 = group2,
                      pmethod = pmethod,
                      alternative = alternative,
                      return.val = 'df',
                      p = NULL,
                      lfc = NULL)
    }

    dea.res = dea.res %>%
        dplyr::mutate(logP = -log10(p.adj)) %>%
        dplyr::mutate(groups = ifelse(logP >= logp & foldchange >= lfc, 1, ifelse(logP >= logp & foldchange <= -1*lfc, 1, 0)))

    if (is.null(selected)) {
        g1 = dea.res %>%
            dplyr::filter(groups == 1) %>%
            dplyr::arrange(desc(foldchange)) %>%
            dplyr::pull(gene)
        g1 = as.character(g1)[1:n]
    
        g2 = dea.res %>%
            dplyr::filter(groups == 1) %>%
            dplyr::arrange(foldchange) %>%
            dplyr::pull(gene)
        g2 = as.character(g2)[1:n]
    
        genes2show = unique(c(g1, g2))
    
        if (!is.null(add.to.selected)) {
            genes2show = unique(c(genes2show, add.to.selected))
        }
    }
    
    else {
        genes2show = unique(selected)
    }

    dea.res = dea.res %>% dplyr::mutate(groups = ifelse(groups == 1 & gene %in% genes2show,2,as.numeric(groups)))
    dea.res = dea.res %>% dplyr::mutate(genes2show = ifelse(groups == 2, gene, ''))
    dea.res$groups = as.character(dea.res$groups)
    labels = setNames(c('Not Significant', 'Significant','Selected'), as.character(0:2))
    labels = labels[unique(dea.res$groups) %>% sort]
    dea.res = dea.res %>% dplyr::mutate(groups = factor(groups, labels = labels))

    palette = setNames(c('grey85', 'grey55', selected.col), c('Not Significant', 'Significant','Selected'))[labels]
    sizes = setNames(c(0.5,0.5,1.5), names(palette))[labels]

    if (is.null(ymax)) {
        ymax = max(dea.res$logP)
    }
    ylim = c(min(dea.res$logP), ymax)

    if (is.null(xlim)) {
        xlim = c(min(dea.res$foldchange), max(dea.res$foldchange))
    }

    if (symmetric) {
        maxi = max(abs(xlim))
        xlim = c(-1 * maxi, maxi)
    }

    if (length(labels) == 1) {
        G = ggplot2::ggplot(dea.res, aes(x = foldchange,
                                     y = logP,
                                     group = groups,
                                     size = groups,
                                     label = genes2show)) +
            ggplot2::geom_point(colour = palette, size = sizes) +
            ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim) + 
            ggplot2::theme_bw() +
            ggrepel::geom_text_repel(size = label.size,
                                      show.legend = FALSE,
                                      colour = palette[2],
                                      segment.colour = palette[1],
                                      force = 10)  +
            ggplot2::theme(legend.title = element_blank(),
                           legend.position = 'top',
                           legend.justification = 'right',
                           legend.text = element_text(size = legend.text.size),
                           text = element_text(size = text.size)) +
            ggplot2::labs(x = xlab, y = ylab)
    }

    else {

        G = ggplot2::ggplot(dea.res, aes(x = foldchange,
                                     y = logP,
                                     group = groups,
                                     size = groups,
                                     label = genes2show)) +
            ggplot2::geom_point(aes(colour = groups)) +
            ggplot2::scale_colour_manual(values = palette) +
            ggplot2::scale_size_manual(values = sizes) +
            ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim) + 
            ggplot2::theme_bw() +
            ggrepel::geom_text_repel(size = label.size,
                                      show.legend = FALSE,
                                      colour = segment.colour,
                                      segment.colour = segment.colour,
                                      segment.size = 0.2,
                                      force = 10)  +
            ggplot2::theme(legend.title = element_blank(),
                           legend.position = 'top',
                           legend.justification = 'right',
                           legend.text = element_text(size = legend.text.size),
                           text = element_text(size = text.size)) +
            ggplot2::labs(x = xlab, y = ylab)
    }

    if (!is.null(xintercept)) {
        G = G + ggplot2::geom_vline(xintercept = xintercept, linetype = 2, colour = 'gray65')
    }

    G

    invisible(list(G = G, dea.res = dea.res))
}
