
volcano_plot = function(
                        m = NULL,
                        group = NULL,
                        group2 = NULL,
                        d = NULL,
                        center.rows = TRUE,
                        vis.lfc = log2(2.5),
                        vis.p = 0.01,
                        xlim = NULL,
                        ylim = NULL,
                        xlab = 'log2 Fold Change',
                        ylab = '-log10(P)',
                        main = 'Volcano Plot',
                        text.cex = .5) {

    Args = list(
                m = m,
                group = group,
                group2 = group2,
                center.rows = center.rows,
                lfc = NULL,
                p = NULL,
                arrange.by = 'none',
                pmethod = 'none',
                return.val = 'df')
#    Args2 = Args
#    Args2$pmethod = 'BH'
    if (is.null(d)) d = do.call(dea, Args)
#    d2 = do.call(dea, Args2)
#    d$p.adj = d2$p.value
    d$p.adj = d$p.value
    d$logp = -log10(d$p.value)
#    browser()
#    rm(d2)
    xname = deparse(quote(group))
    yname = deparse(quote(group2))

    with(d, plot(foldchange,
                 logp,
                 pch=20,
                 main=main,
                 xlim=xlim,
                 ylim=ylim,
                 xlab = xlab,
                 ylab = ylab))

    with(subset(d, p.adj < vis.p), points(foldchange,
                                          logp,
                                          pch=20,
                                          col="red"))

    with(subset(d, abs(foldchange) > vis.lfc), points(foldchange,
                                                      logp,
                                                      pch=20,
                                                      col="orange"))

    with(subset(d, abs(foldchange) > vis.lfc & p.adj < vis.p), points(foldchange,
                                                                      logp,
                                                                      pch=20,
                                                                      col="green"))
    
    
    with(subset(d, p.adj < vis.p & abs(foldchange) > vis.lfc), textxy(foldchange,
                                                                      logp,
                                                                      labs = gene,
                                                                      cex = text.cex,
                                                                      pos = 4))

#    d3 = dplyr::filter(d, p.adj >= vis.p & p.adj < 0.05 & abs(foldchange) <= vis.lfc & abs(foldchange) > log2(2))
#    with(d3, textxy(foldchange,
#                               logp,
#                               labs = gene,
#                               cex = .3))
    invisible(d)
}
