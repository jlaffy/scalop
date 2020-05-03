
#' @title Plot model and outliers 
#' @description Plot model and outliers 
#' @param formula PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param mod PARAM_DESCRIPTION, Default: loess
#' @param n.sd PARAM_DESCRIPTION, Default: 1.5
#' @param ... PARAM_DESCRIPTION
#' @param selected PARAM_DESCRIPTION, Default: NULL
#' @param add.to.selected PARAM_DESCRIPTION, Default: NULL
#' @param ngenes PARAM_DESCRIPTION, Default: 7
#' @param yvar.ngenes PARAM_DESCRIPTION, Default: ngenes
#' @param xvar.ngenes PARAM_DESCRIPTION, Default: ngenes
#' @param gene.min PARAM_DESCRIPTION, Default: 3
#' @param alt.min PARAM_DESCRIPTION, Default: 1
#' @param xlab.min PARAM_DESCRIPTION, Default: 0
#' @param ylab.min PARAM_DESCRIPTION, Default: 0
#' @param text.size PARAM_DESCRIPTION, Default: 14
#' @param legend.text.size PARAM_DESCRIPTION, Default: 12
#' @param label.size PARAM_DESCRIPTION, Default: 5
#' @param force PARAM_DESCRIPTION, Default: 20
#' @param xlab PARAM_DESCRIPTION, Default: all.vars(formula)[2]
#' @param ylab PARAM_DESCRIPTION, Default: all.vars(formula)[1]
#' @param selected.col PARAM_DESCRIPTION, Default: 'red'
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param caption PARAM_DESCRIPTION, Default: NULL
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
#'  \code{\link[stats]{setNames}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggrepel]{geom_text_repel}}
#' @rdname ggoutliers
#' @export 
#' @importFrom dplyr mutate filter arrange pull
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot geom_point geom_line scale_x_continuous scale_y_continuous theme_bw scale_colour_manual scale_size_manual theme
#' @importFrom ggrepel geom_text_repel
ggoutliers = function(formula,
                      data,
                      mod = loess,
                      n.sd = 1.5,
                      ...,
                      selected = NULL,
                      add.to.selected = NULL,
                      ngenes = 6,
                      yvar.ngenes = ngenes,
                      xvar.ngenes = ngenes,
                      gene.min = 0.5,
                      alt.min = 0.5,
                      xlab.min = 0,
                      ylab.min = 0,
                      text.size = 14,
                      legend.text.size = 12,
                      label.size = 3,
                      force = 20,
                      xlab = all.vars(formula)[2],
                      ylab = all.vars(formula)[1],
                      selected.col ='red',
                      subtitle = NULL,
                      title = NULL,
                      caption = NULL) {

    data = outliers(formula = formula, data = data, mod = mod, n.sd = n.sd, ...)


    if (is.null(selected)) {

        g2show = c()
        if (!is.null(yvar.ngenes)) {
            y2show = data %>%
                dplyr::mutate(gene = as.character(gene)) %>%
                dplyr::filter(y > gene.min, x > alt.min) %>%
                dplyr::arrange(desc(resid.high)) %>%
                dplyr::pull(gene)
            y2show = y2show[1:min(yvar.ngenes, length(y2show))]
            g2show = unique(c(g2show, y2show))
        }

        if (!is.null(xvar.ngenes)) {
            x2show = data %>%
                dplyr::mutate(gene = as.character(gene)) %>%
                dplyr::filter(y > alt.min, x > gene.min) %>%
                dplyr::arrange(desc(resid.low)) %>%
                dplyr::pull(gene)
            x2show = x2show[1:min(xvar.ngenes, length(x2show))]
            g2show = unique(c(g2show, x2show))
        }

        if (!is.null(add.to.selected)) {
            g2show = unique(c(g2show, add.to.selected))
        }
    }
    
    else {
        g2show = unique(selected)
    }

    data = data %>% dplyr::mutate(genes2show = ifelse(gene %in% g2show, gene, ''))
    data = data %>% dplyr::mutate(group = ifelse(y < ylower | y > yupper, 1, 0))
    data = data %>% dplyr::mutate(group = ifelse(genes2show == gene & group == 1, 2, as.numeric(group)))
    data = data %>% dplyr::mutate(group = factor(group, labels = c('Normal', 'Outlier', 'Selected')))
    palette = c(Normal = 'black', Outlier = 'black', Selected = selected.col)
    #palette = c(Normal = 'grey85', Outlier = 'grey55', Selected = selected.col)
    #sizes = stats::setNames(c(1.5, 1.5, 2.5), names(palette))
    sizes = stats::setNames(c(0.2, 0.5, 1), names(palette))
    ylim = c(ylab.min, max(max(data$y), max(data$yupper)))
    xlim = c(xlab.min, max(data$x))

    invisible(data)

    ggplot2::ggplot(data, aes(x = x,
                              y = y,
                              colour = group,
                              size = group,
                              label = genes2show)) +
        ggplot2::geom_point() +
#         ggplot2::geom_line(aes(y = yupper), colour = palette[2], size = 0.75) +
#         ggplot2::geom_line(aes(y = ylower), colour = palette[2], size = 0.75) +
#         ggplot2::geom_line(aes(y = ymid), colour = palette[2], linetype = 3, size = 1) +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim) + 
        ggplot2::theme_bw() +
        ggrepel::geom_text_repel(size = label.size,
                                  show.legend = FALSE,
                                  colour = palette[2],
                                  segment.colour = 'transparent',
                                  #segment.colour = palette[1],
                                  force = force)  +
        ggplot2::scale_colour_manual(values = palette) +
        ggplot2::scale_size_manual(values = sizes) +
        ggplot2::theme(legend.title = element_blank(),
                       legend.position = 'top',
                       legend.justification = 'right',
                       legend.text = element_text(size = legend.text.size),
                       text = element_text(size = text.size)) +
        labs(y = ylab, x = xlab, subtitle = subtitle, caption = caption, title = title)
}
