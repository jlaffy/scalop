# SO answer by Hong Ooi:
# r-pass-optional-arguments-to-nested-functions
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mod PARAM_DESCRIPTION
#' @param formula PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname genericModel
#' @export 
genericModel <- function(mod, formula, data, ...) {
    cl <- match.call(expand=TRUE)
    cl[[1]] <- cl$mod
    cl$mod <- NULL
    eval(cl, parent.frame())
}

# genericModel(lm, mpg ~ hp, data=mtcars, weights=gear)
# genericModel(glm, Volume ~ Girth + Height, data=trees, family=Gamma(link=log))

