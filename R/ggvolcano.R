
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION, Default: NULL
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @param group2 PARAM_DESCRIPTION, Default: NULL
#' @param dea.res PARAM_DESCRIPTION, Default: NULL
#' @param selected PARAM_DESCRIPTION, Default: NULL
#' @param add.to.selected PARAM_DESCRIPTION, Default: NULL
#' @param selected.col PARAM_DESCRIPTION, Default: 'red'
#' @param alternative PARAM_DESCRIPTION, Default: NULL
#' @param xlab PARAM_DESCRIPTION, Default: 'Log2 Fold Change'
#' @param ylab PARAM_DESCRIPTION, Default: '- Log10 P-value (adjusted)'
#' @param pmethod PARAM_DESCRIPTION, Default: 'BH'
#' @param ngenes PARAM_DESCRIPTION, Default: 10
#' @param text.size PARAM_DESCRIPTION, Default: 14
#' @param label.size PARAM_DESCRIPTION, Default: 5
#' @param symmetric PARAM_DESCRIPTION, Default: TRUE
#' @param add.line PARAM_DESCRIPTION, Default: TRUE
#' @param ymax PARAM_DESCRIPTION, Default: NULL
#' @param xlim PARAM_DESCRIPTION, Default: NULL
#' @param legend.text.size PARAM_DESCRIPTION, Default: 12
#' @return OUTPUT_DESCRIPTION
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
#'  \code{\link[ggrepel]{geom_label_repel}}
#' @rdname ggvolcano
#' @export 
#' @importFrom dplyr mutate filter arrange pull
#' @importFrom ggplot2 ggplot geom_point scale_x_continuous scale_y_continuous theme_bw theme labs scale_colour_manual scale_size_manual geom_vline
#' @importFrom ggrepel geom_label_repel
ggvolcano = function(
                     m = NULL,
                     group = NULL,
                     group2 = NULL,
                     dea.res = NULL,
                     selected = NULL,
                     add.to.selected = NULL,
                     selected.col = 'red',
                     alternative = NULL,
                     xlab = 'Log2 Fold Change',
                     ylab = '- Log10 P-value (adjusted)',
                     pmethod = 'BH',
                     ngenes = 10,
                     text.size = 14,
                     label.size = 5,
                     symmetric = TRUE,
                     add.line = TRUE, 
                     ymax = NULL,
                     xlim = NULL,
                     legend.text.size = 12) {

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
        dplyr::mutate(groups = ifelse(logP >= 2 & foldchange >= 1, 1, ifelse(logP >= 2 & foldchange <= -1, 1, 0)))

    if (is.null(selected)) {
        g1 = dea.res %>%
            dplyr::filter(groups == 1) %>%
            dplyr::arrange(desc(foldchange)) %>%
            dplyr::pull(gene)
        g1 = as.character(g1)[1:ngenes]
    
        g2 = dea.res %>%
            dplyr::filter(groups == 1) %>%
            dplyr::arrange(foldchange) %>%
            dplyr::pull(gene)
        g2 = as.character(g2)[1:ngenes]
    
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
    sizes = setNames(c(1.5,1.5,2.5), names(palette))[labels]

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
            ggrepel::geom_label_repel(size = label.size,
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
            ggrepel::geom_label_repel(size = label.size,
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

    if (add.line) {
        G = G + ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = 'gray65')
    }

    G

    invisible(list(G = G, dea.res = dea.res))
}
