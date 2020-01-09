
jacmap = function(L,
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

    d = reshape2::melt(hca_reorder(jaccard(L)))

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

    G
}
