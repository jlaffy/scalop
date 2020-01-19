
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param xlab PARAM_DESCRIPTION, Default: 'Cell Score'
#' @param mode.names PARAM_DESCRIPTION, Default: c("Mode1", "Mode2")
#' @param ... PARAM_DESCRIPTION
#' @param prob PARAM_DESCRIPTION, Default: 0.95
#' @param coverage PARAM_DESCRIPTION, Default: 0.95
#' @param min.size PARAM_DESCRIPTION, Default: 10
#' @param na.action PARAM_DESCRIPTION, Default: c("show", "hide", "combine")
#' @param combine.with PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stats]{setNames}}
#'  \code{\link[ggpubr]{ggdensity}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}}
#' @rdname ggDensity
#' @export 
#' @importFrom stats setNames
#' @importFrom ggpubr ggdensity
#' @importFrom dplyr filter mutate
ggDensity = function(data,
                     x,
                     xlab = 'Cell Score',
                     mode.names = c('Mode1', 'Mode2'),
                     
                     ...,
                     prob = 0.95,
                     coverage = 0.95,
                     min.size = 10,
                     na.action = c('show','hide','combine'),
                     combine.with = 1) {

    na.action = match.arg(na.action)
    vals = stats::setNames(data[[x]], data[[1]])
    modes = fitBimodal(vals,
                       assign = T,
                       coverage = coverage,
                       prob = prob,
                       size = min.size)

    if (length(modes) == 1) {
        return(ggpubr::ggdensity(data, x = x, ...))
    }

    names(modes) <- mode.names
    nas <- names(vals)[!names(vals) %in% unlist(modes)]
    modes = c(modes, list(`NA`=nas))
    modes0 = modes
    modes = Unlist(modes)
    data = add_to_dataframe(data,
                            vec = flip(modes),
                            by = colnames(data)[1],
                            newcol = 'Mode')
    #palette =c("#00AFBB", "#E7B800", "#FC4E07")
    if (na.action == 'hide') {
        data = data %>%
            dplyr::filter(Mode != 'NA')
    }

    else if (na.action == 'combine') {
        data = data %>%
            dplyr::mutate(Mode = ifelse(Mode == 'NA',
                                        mode.names[combine.with],
                                        as.character(Mode)))
    }

    G <- ggpubr::ggdensity(data,
                           x = x,
                           xlab = xlab,
                           col = 'Mode',
                           ylab = 'Density',
                           fill = 'Mode',
                           rug = T,
                           add = 'mean',
                           ...)

    plot(G)
    return(invisible(list(data = data, modes = modes0)))
}
