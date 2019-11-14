
#' @title Load Example Data
#' @description Load example dataset of human single-cell RNA seq data. Cells and genes are columns and rows, respectively, and all passed QC. Matrix is row-centered.
#' @param rowcenter center the expression matrix row-wise. Default: F
#' @return centered expression matrix of genes X cells 
#' @rdname useData
#' @export 
useData = function(rowcenter = F) {
    x = as.matrix(bt771)
    if (rowcenter) x = rowCenter(x)
    x
}
