# remember m should be transposed
# such that variables (genes) are columns (cells) are rows 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param npcs PARAM_DESCRIPTION, Default: 100
#' @param retx PARAM_DESCRIPTION, Default: TRUE
#' @param center PARAM_DESCRIPTION, Default: TRUE
#' @param grouped.center PARAM_DESCRIPTION, Default: NULL
#' @param scale PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[irlba]{prcomp_irlba}}
#' @rdname pca
#' @export 
#' @importFrom irlba prcomp_irlba
pca = function(m,
               npcs = 100,
               retx = TRUE,
               center = TRUE,
               grouped.center = NULL,
               scale = FALSE) {

    if (!is.null(grouped.center)) {
        center = FALSE
        m = t(grouped_rowcenter(t(m), groups = grouped.center, by = 'mean'))
    }

    m.pca <- irlba::prcomp_irlba(m,
                                 n = npcs,
                                 retx = retx,
                                 center = center,
                                 scale = scale)

    rownames(m.pca$x) <- rownames(m)
    rownames(m.pca$rotation) <- colnames(m)
    m.pca
}

