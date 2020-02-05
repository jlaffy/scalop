
#' @title Center matrix rows by subsets of columns 
#' @description  
#' @param m a matrix
#' @param groups list of character vectors specifying the column ids to center by.  Default: split_by_sample_names(colnames(m))
#' @param by how centering should be performed. One of 'mean' or 'median'. Default: 'mean' 
#' @return A row-centered matrix, centered within groups and with the order of cells within groups retained. The matrix has the same number of rows as <m> and as many columns as there are cell ids in groups. 
#' @rdname grouped_rowcenter 
#' @export 
grouped_rowcenter = function(m,
                             groups = split_by_sample_names(colnames(m)),
                             by = 'mean') {

    res = sapply(groups, function(cells) {
                     rowcenter(m[, cells], by = by)},
                     simplify = F)
    Names = unlist(sapply(res, colnames, simplify = F))
    res = do.call(cbind.data.frame, res)
    colnames(res) <- Names
    res
}
