
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param formula PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param mod PARAM_DESCRIPTION, Default: loess
#' @param n.sd PARAM_DESCRIPTION, Default: 1.5
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
#'  \code{\link[dplyr]{mutate}}
#' @rdname outliers
#' @export 
#' @importFrom dplyr mutate
outliers = function(formula, data, mod = loess, n.sd = 1.5, ...) {
    mod = genericModel(mod, formula, data, ...)
    data$resid = mod$residuals
    data$ymid = predict(mod)
    y.sd = sd(data$ymid)
    data$yupper = data$ymid + (n.sd * y.sd)
    data$ylower = data$ymid - (n.sd * y.sd)
    columns = all.vars(formula)[1:2]
    colnames(data)[which(colnames(data) == columns[[1]])] <- 'y'
    colnames(data)[which(colnames(data) == columns[[2]])] <- 'x'
    data = data %>% dplyr::mutate(resid.high = y - yupper, resid.low = -1 * (y - ylower))
    data
}

