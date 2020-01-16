
ggBimodalDensity = function(data, x, fit.bimodal = FALSE, ...) {
    if (fit.bimodal) {
        vals = stats::setNames(data[[x]], rownames(data))
        modes = fitBimodal(vals, assign = T, ...)
        if (length(modes) == 1) return(ggpubr::ggdensity(data, x = x))
    }

    names(modes) <- c('Mode1', 'Mode2')
    modes = stats::setNames(rep(names(modes), lengths(modes)),unlist(modes))
    data$Mode = modes[rownames(data)]
    palette =c("#00AFBB", "#E7B800", "#FC4E07")
    ggpubr::ggdensity(data, x = x, col = 'Mode', fill = 'Mode', rug = T, add = 'mean')
}
