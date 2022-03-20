
.limit <- function(v) {
    
    m = 10
    Sign = sign(v)
    v = abs(v)
    v.lim = Sign * (floor(m * v) / m)
    return(v.lim)
}

.limits <- function(v, symmetric = TRUE) {
    stopifnot(length(v) == 2, all(sapply(v, class) == 'numeric'))

    if (!symmetric) {
        return(sapply(v, .limit))
    }
    
    limit = .limit(min(abs(v)))
    return(c(-1 * limit, 1 * limit))
}


.axis.spacer <- function(breaks, labels, limits, levels = NULL) {
    if (!is.null(labels) & !is.null(levels)) {
        breaks = levels %in% labels
        labels = levels[breaks]
    }
    if (is.null(breaks)) {
        breaks = seq(limits[[1]], limits[[2]], limits[[2]])
    }
    if (is.null(labels)) {
        labels = breaks
    }
    return(list(breaks = breaks, labels = labels))
}


#' @title Plot A Heatmap with ggplot2
#' @export
#' @rdname gmap
gmap <- function(dat,
                 x = Var1,
                 y = Var2,
                 fill = value,
                 tile.labels = FALSE,
                 limits = c(-0.5, 0.5),
                 lim.find = F,
                 lim.sym = T,
                 x.name = NULL,
                 y.name = NULL,
                 angle = NULL,
                 axis.rel = 1,
                 title.rel = 1.1,
                 title = NULL,
                 subtitle = NULL,
                 caption = NULL,
                 text.size = 12,
                 ratio = NULL,
                 tile.size = 0.1,
                 tile.col = NULL,
                 cols = NULL,
                 col = NULL,
                 na.value = 'white',
                 legend.position = 'right',
                 legend.justification = 'bottom',
                 legend.height = 0.4,
                 legend.width = 0.6,
                 legend.rel = 0.8,
                 legend.colour = 'black',
                 ticks.linewidth = 0.5,
                 breaks = waiver(),
                 labels = waiver(),
                 x.breaks = waiver(),
                 x.axis.position = 'bottom',
                 y.breaks = waiver(),
                 x.labels = waiver(),
                 y.labels = waiver(),
                 num = T,
                 y.num = num, 
                 x.num = num,
                 label.col = 'black',
                 legend.breaks = NULL,
                 legend.labels = NULL,
                 legend.title = NULL,
                 legend.title.rel = 0.8,
                 expand = c(0,0)) {
    
    x = rlang::enquo(x)
    y = rlang::enquo(y)
    fill = rlang::enquo(fill)
    xname = rlang::quo_name(x)
    yname = rlang::quo_name(y)

    if (class(breaks) != 'waiver') {
        x.breaks = breaks
        y.breaks = breaks
    }

    if (class(labels) != 'waiver') {
        x.labels = labels
        y.labels = labels
    }

    if (x.num | num | class(dat %>% dplyr::pull(!!x)) == 'numeric') {
        dat = dat %>% dplyr::mutate(!!xname := as.numeric(!!x))
        x.scale.FUN = ggplot2::scale_x_continuous
    } else {
        x.scale.FUN = ggplot2::scale_x_discrete
    }
    if (y.num | num | class(dat %>% dplyr::pull(!!y)) == 'numeric') {
        dat = dat %>% dplyr::mutate(!!yname := as.numeric(!!y))
        y.scale.FUN = ggplot2::scale_y_continuous
    } else {
        y.scale.FUN = ggplot2::scale_y_discrete
    }

    x.scale = quote(x.scale.FUN(expand = expand,
                                breaks = x.breaks,
                                labels = x.labels))
    y.scale = quote(y.scale.FUN(expand = expand,
                                breaks = y.breaks,
                                labels = y.labels))


    if (!is.null(col)) {
        cols = col
        legend.position = 'none'
    }

    if (isTRUE(tile.labels)) {
        dat = dat %>% dplyr::mutate(label = round(!!fill, 1))
    }

    else if (!is.null(tile.labels) & length(tile.labels) == nrow(dat)) {
        dat$label = tile.labels
    }

    if (lim.find) {
        v = dat %>% dplyr::select(!!fill) %>% range
        limits = .limits(v = v, symmetric = lim.sym)
    }

    if (is.null(cols) & limits[[1]] >= 0) {
        cols = RColorBrewer::brewer.pal(9, 'YlOrRd')
    } else if (is.null(cols)) {
        cols = c("#053061",
                 "#2166ac",
                 "#4393c3",
                 "#92c5de", 
                 "#d1e5f0",
                 "#f7f7f7",
                 "#fddbc7",
                 "#f4a582",
                 "#d6604d",
                 "#b2182b",
                 "#67001f")
    }

    if (isTRUE(angle)) angle = 45

    if (is.numeric(angle)) angle = ggpubr::rotate_x_text(angle = angle, vjust = 1)

    legend = .axis.spacer(breaks = legend.breaks, labels = legend.labels, limits = limits)
    legend.breaks = legend$breaks
    legend.labels = legend$labels
    
    geomtile = geom_tile(col = tile.col, size = tile.size)
    if (any(sapply(list(tile.size, tile.col), is.null))) {
        geomtile = ggplot2::geom_tile()
    }

    G <- ggplot2::ggplot(dat, aes(x = !!x, y = !!y, fill = !!fill, group = 1)) +
        geomtile +
        ggplot2::scale_fill_gradientn(colors = cols,
                                      limits = limits,
                                      expand = expand,
                                      oob = scales::squish,
                                      breaks = legend.breaks,
                                      labels = legend.breaks,
                                      name = legend.title,
                                      na.value = na.value,
                                      guide = ggplot2::guide_colorbar(frame.colour='black',
								      ticks.colour='black',
								      title.position='top',
								      title.hjust=0.5,
								      barwidth=3,
								      barheight=0.75)) +
        ggplot2::labs(x = x.name,
                      y = y.name,
                      title = title,
                      subtitle = subtitle,
                      caption = caption) +
        ggplot2::theme_bw(base_size = text.size) +
        angle +
        ggplot2::theme(aspect.ratio = ratio,
                       panel.grid = ggplot2::element_blank(),
                       title = ggplot2::element_text(size = ggplot2::rel(title.rel)),
                       axis.title = ggplot2::element_text(size = ggplot2::rel(axis.rel)),
                       legend.position = legend.position,
                       legend.justification = legend.justification,
                       legend.text = ggplot2::element_text(size = ggplot2::rel(legend.rel),
                                                           colour = legend.colour,
                                                           hjust = 0.5),
                       legend.title = ggplot2::element_text(size = ggplot2::rel(legend.title.rel)),
                       legend.margin = ggplot2::margin(t = -0.5, unit='cm'),
                       legend.key.height = grid::unit(legend.height, "cm"),
                       legend.key.width = grid::unit(legend.width, "cm")) +
        eval(x.scale) +
        eval(y.scale)

    if ("label" %in% colnames(dat)) {
        G = G + ggplot2::geom_text(aes(label = label), colour = label.col)
    }

    if (x.axis.position == 'top') {
        G = G + ggplot2::scale_x_discrete(position = 'top',expand = c(0,0))
    }
    
    G
}

# mapAssign <- function(dat,
#                       x,
#                       fill,
#                       x.name = NULL,
#                       y.name = NULL,
#                       caption = NULL,
#                       subtitle = NULL,
#                       title = NULL,
#                       cols = NULL,
#                       ratio = 1,
#                       x.breaks = waiver(),
#                       x.num = F,
#                       expand = c(0,0),
#                       horiz = T) {
# 
#     x = rlang::enquo(x)
#     fill = rlang::enquo(fill)
#     xname = rlang::quo_name(x)
# #    yname = rlang::quo_name(y)
# 
#     if (class(breaks) != 'waiver') {
#         x.breaks = breaks
#         y.breaks = breaks
#     }
# 
#     x.scale = quote(ggplot2::scale_x_discrete(expand = expand, breaks = x.breaks))
#     if (x.num | num | class(unlist(dat %>% dplyr::select(!!x))) == 'numeric') {
#         dat = dat %>% dplyr::mutate(!!xname := as.numeric(!!x))
#         x.scale = quote(ggplot2::scale_x_continuous(expand = expand, breaks = x.breaks))
#     }
# 
#     y.scale = quote(ggplot2::scale_y_continuous(expand = expand, breaks = NULL))
# 
#     ggplot2::ggplot(dat, aes(x = !!x, fill = !!fill, y = 1)) +
#         ggplot2::geom_tile() + 
#         ggplot2::theme_bw() +
#         ggplot2::theme(aspect.ratio = ratio) +
#         eval(x.scale) +
#         eval(y.scale) +
#         ggplot2::labs(x = x.name,
#                       y = y.name,
#                       title = title,
#                       subtitle = subtitle,
#                       caption = caption) +
#         ggplot2::scale_fill_manual(values = rainbow(n = 5))
# }
# 
# 
# map1bar <- function(dat,
#                     x,
#                     y,
#                     fill = 1,
#                     limits = c(-0.5, 0.5),
#                     lim.find = F,
#                     lim.sym = T,
#                     x.name = NULL,
#                     y.name = NULL,
#                     angle = NULL,
#                     axis.rel = 1,
#                     title.rel = 1.1,
#                     title = NULL,
#                     subtitle = NULL,
#                     caption = NULL,
#                     text.size = 12,
#                     ratio = NULL,
#                     tile.size = 0.1,
#                     tile.col = NULL,
#                     cols = NULL,
#                     col = NULL,
#                     legend.position = 'right',
#                     legend.height = 0.2,
#                     legend.width = 0.15,
#                     legend.rel = 0.6,
#                     legend.colour = 'black',
#                     ticks.linewidth = 0.5,
#                     breaks = waiver(),
#                     x.breaks = waiver(),
#                     y.breaks = waiver(),
#                     num = F,
#                     y.num = T, 
#                     x.num = T,
#                     legend.breaks = NULL,
#                     legend.labels = NULL,
#                     legend.title = NULL,
#                     legend.title.rel = 0.6,
#                     expand = c(0,0)) {
#     
#     x = rlang::enquo(x)
#     y = rlang::enquo(y)
#     fill = rlang::enquo(fill)
#     xname = rlang::quo_name(x)
#     yname = rlang::quo_name(y)
# 
#     if (class(breaks) != 'waiver') {
#         x.breaks = breaks
#         y.breaks = breaks
#     }
# 
#     x.scale = quote(ggplot2::scale_x_discrete(expand = expand, breaks = x.breaks))
#     y.scale = quote(ggplot2::scale_y_discrete(expand = expand, breaks = y.breaks))
# 
#     if (x.num | num | class(unlist(dat %>% dplyr::select(!!x))) == 'numeric') {
#         dat = dat %>% dplyr::mutate(!!xname := as.numeric(!!x))
#         x.scale = quote(ggplot2::scale_x_continuous(expand = expand, breaks = x.breaks))
#     }
#     if (y.num | num | class(unlist(dat %>% dplyr::select(!!y))) == 'numeric') {
#         dat = dat %>% dplyr::mutate(!!yname := as.numeric(!!y))
#         y.scale = quote(ggplot2::scale_y_continuous(expand = expand, breaks = breaks))
#     }
# 
#     if (!is.null(col)) {
#         cols = col
#         legend.position = 'none'
#     }
# 
#     if (is.null(cols)) {
#         cols = readRDS('~/rds/hotmap.rds')
#     }
# 
#     if (lim.find) {
#         v = dat %>% dplyr::select(!!fill) %>% range
#         limits = .limits(v = v, symmetric = lim.sym)
#     }
# 
#     if (isTRUE(angle)) {
#         angle = 45
#     }
# 
#     if (is.numeric(angle)) {
#         angle = ggpubr::rotate_x_text(angle = angle, vjust = 1)
#     }
# 
#     legend = .axis.spacer(breaks = legend.breaks, labels = legend.labels, limits = limits)
#     legend.breaks = legend$breaks
#     legend.labels = legend$labels
#     
#     geomtile = geom_tile(col = tile.col, size = tile.size)
#     if (any(sapply(list(tile.size, tile.col), is.null))) {
#         geomtile = ggplot2::geom_tile()
#     }
# 
#     G <- ggplot2::ggplot(dat, aes(x = !!x, y = !!y, fill = !!fill, group = 1)) +
#         geomtile +
#         ggplot2::scale_fill_gradientn(colors = cols,
#                                       limits = limits,
#                                       expand = expand,
#                                       oob = scales::squish,
#                                       breaks = legend.breaks,
#                                       labels = legend.breaks,
#                                       name = legend.title,
#                                       guide = ggplot2::guide_colorbar(frame.colour = 'black')) +
#         ggplot2::labs(x = x.name,
#                       y = y.name,
#                       title = title,
#                       subtitle = subtitle,
#                       caption = caption) +
#         ggplot2::theme_bw(base_size = text.size) +
#         angle +
#         ggplot2::theme(aspect.ratio = ratio,
#                        panel.grid = ggplot2::element_blank(),
#                        title = ggplot2::element_text(size = ggplot2::rel(title.rel)),
#                        axis.title = ggplot2::element_text(size = ggplot2::rel(axis.rel)),
#                        legend.position = legend.position,
#                        legend.text = ggplot2::element_text(size = ggplot2::rel(legend.rel),
#                                                            colour = legend.colour,
#                                                            hjust = 0.5),
#                        legend.title = ggplot2::element_text(size = ggplot2::rel(legend.title.rel)),
#                        legend.margin = ggplot2::margin(t = -0.5, unit='cm'),
#                        legend.key.height = grid::unit(legend.height, "cm"),
#                        legend.key.width = grid::unit(legend.width, "cm")) +
#         eval(x.scale) +
#         eval(y.scale)
# 
#     G
# }
# 
