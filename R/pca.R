
#' @title Principal Component Analysis 
#' @description A wrapper for irlba::prcomp_irlba(). Performs efficient computation of a principal component analysis of a given data matrix. 
#' @param m A numeric matrix or data frame.
#' @param npcs The number of principal component vectors to return. Must be less than 'min(dim(m))' Default: 100
#' @param retx A logical value indicating whether the rotated variables should be returned. Default: TRUE
#' @param center A logical value indicating whether the variables should be shifted to be zero centered. Alternately, a centering vector of length equal to the number of columns of 'm' can be supplied. Default: TRUE
#' @param scale A logical value indicating whether the variables should be scaled to have unit variance before the analysis is performed. The value of 'scale' determines how column scaling is performed (after centering). If 'scale' is a numeric vector with length equal to the number of columns of 'm', then each column of 'm' is divided by the corresponding value from 'scale'. If 'scale' is 'TRUE', then scaling is done by dividing the (centered) columns of 'm' by their standard deviations if 'center=TRUE', and the root mean square otherwise.  If 'scale' is 'FALSE', no scaling is done. Default: FALSE
#' @return A list of class "prcomp" containing the following components:
#' \itemize{
#' \item {sdev:} {the standard deviations of the principal components (i.e. the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).}
#' \item {rotation:} {the matrix of variable loadings (i.e. a matrix whose columns contain the eigenvectors).}
#' \item {x:} {if 'retx' is 'TRUE' the value of the rotated data (the centered (and scaled if requested) data multipleied by the 'rotation' matrix) is returned. Hence, 'cov(m)' is the diagonal matrix 'diag(sdev^2)'.}
#' \item {center, scale:} {the centering and scaling used, or 'FALSE'.}
#' }
#' @details see 'irlba::prcomp_irlba()' for more details. 
#' @seealso 
#'  \code{\link[irlba]{prcomp_irlba}}
#' @rdname pca
#' @export 
#' @importFrom irlba prcomp_irlba
pca = function(m,
               npcs = 100,
               retx = TRUE,
               center = TRUE,
               scale = FALSE) {

    pc.obj <- irlba::prcomp_irlba(m,
                                 n = npcs,
                                 retx = retx,
                                 center = center,
                                 scale. = scale)

    rownames(pc.obj$x) <- rownames(m)
    pc.obj
}

