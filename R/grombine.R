#' @title Combine ggplot grobs or plots
#' @description combine ggplot grobs or plots
#' @param ... plots/grobs to combine. A mix of the two is fine.
#' @param by axis to combine plots by. Default: 'x'
#' @param size which plot's dimensions to prioritise. One of 'first' or 'last'. Default: 'last'
#' @param draw logical to return plot instead of grob. Default: T
#' @return ggplot plot or grob object, with all objects in ... combined.
#' @seealso 
#'  \code{\link[ggplot2]{ggplotGrob}}
#'  \code{\link[grid]{grid.draw}}
#' @rdname grombine
#' @export 
grombine <- function(..., by = 'x', size = 'last', draw = T) {
    if (by %in% c('y', 'column')) {
        FUN.count = nrow
        FUN.add = .add_rows
        FUN = cbind
    }

    else if (by %in% c('ro', 'x')) {
        FUN.count = ncol
        FUN.add = .add_cols
        FUN = rbind
    }

    dots <- list(...)
    grobs <- sapply(dots, function(obj) ifelse(.is_grob(obj), return(obj), return(ggplot2::ggplotGrob(obj))), simplify = F)
    grobs <- .equalize_grob_dim(grobs, FUN.count = FUN.count, FUN.add = FUN.add)
    args <- c(grobs, list(size = size))
    Grob <- do.call(what = FUN, args = args)
    if (!draw) return(Grob)
    grid::grid.draw(Grob)
}



.marrangegrob <- function(...,
                          by = 'x',
                          size = 'last',
                          draw = FALSE,
                          nrow = 2,
                          ncol = 2,
                          marrange = F) {

    dots <- list(...)
    grobs <- sapply(dots, function(obj) {
                        ifelse(is_grob(obj), return(obj), return(ggplotGrob(obj)))}
        , simplify = F)

    Grob = gridExtra::marrangeGrob(grobs = grobs, nrow = nrow, ncol = ncol)
    if (!draw) return(Grob)
    grid::grid.draw(Grob)
}
