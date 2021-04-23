
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param title.size PARAM_DESCRIPTION, Default: 16
#' @param text.size PARAM_DESCRIPTION, Default: 14
#' @param axis.title.size PARAM_DESCRIPTION, Default: title.size
#' @param legend.title.size PARAM_DESCRIPTION, Default: title.size
#' @param strip.text.size PARAM_DESCRIPTION, Default: title.size
#' @param axis.text.size PARAM_DESCRIPTION, Default: text.size
#' @param legend.text.size PARAM_DESCRIPTION, Default: text.size
#' @param legend.position PARAM_DESCRIPTION, Default: 'bottom'
#' @param legend.justification PARAM_DESCRIPTION, Default: 'right'
#' @param text.colour PARAM_DESCRIPTION, Default: 'black'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}}
#'  \code{\link[grid]{unit}}
#' @rdname theme_scalop
#' @export 
#' @importFrom ggplot2 theme_bw theme element_text element_blank margin
#' @importFrom grid unit
theme_scalop = function(title.size = 16,
                        text.size = 14,
                        axis.title.size = title.size,
                        legend.title.size = title.size,
                        strip.text.size = title.size,
                        axis.text.size = text.size,
                        legend.text.size = text.size,
                        legend.position = 'bottom',
                        legend.justification = 'right',
                        text.colour = 'black',
                        ...) {

    ggplot2::theme_bw() +
        ggplot2::theme(...) +
        ggplot2::theme(text = ggplot2::element_text(size = text.size, colour = text.colour),
                       axis.text = ggplot2::element_text(size = axis.text.size, colour = text.colour),
                       axis.title = ggplot2::element_text(size = axis.title.size, colour = text.colour),
                       strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(size = strip.text.size, colour = text.colour),
                       legend.text = ggplot2::element_text(size = legend.text.size, colour = text.colour),
                       legend.title = ggplot2::element_text(size = legend.title.size, colour = text.colour),
                       legend.position = legend.position,
                       legend.justification = legend.justification,
                       #panel.grid = ggplot2::element_blank(),
                       axis.ticks.length = grid::unit(-0.15, 'cm'),
                       axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.35, unit = 'cm')),
                       axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.35, unit = 'cm')))
}

