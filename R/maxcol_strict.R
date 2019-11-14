#' @title Strict Column Assign
#' @description For each column, returns a vector of the rownames whose values were highest. The "snobby" part comes if the user provides values for min and diff, which require that a row's max value be higher than or equal to <min> and with a difference greater than or equal to <diff> in order to be assigned to a column. Rows that do not pass these criteria are ignored. 
#' @param mat a matrix
#' @param min minimum value required for a row to be assigned to a column
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column.
#' @param splitByCol a boolean value indicating whether the resulting vector should be split by column variable. Default = FALSE
#' @return a vector of the rownames whose values were highest in that column, or if splitByCol is TRUE, a list. 
#' @rdname maxcol_strict
#' @export 
maxcol_strict <- function(mat, min = NULL, diff = NULL, splitByCol = FALSE) {
    # assigns each row to one of the column variables or to nothing
    # returns rownames by column variable in a list
    # max column

    if (is.null(dim(mat))) stop('Please provide a matrix or dataframe.')
    if (is.null(diff)) diff = -Inf
    if (is.null(min)) min = -Inf

    minbool = quote(max(row) >= min)
    diffbool = quote(sort(row, decreasing = T)[1] - sort(row, decreasing = T)[2] >= diff)
    bool = apply(mat, 1, function(row) eval(minbool) & eval(diffbool))
    maxes = stats::setNames(rownames(mat[bool,]), colnames(mat)[max.col(mat[bool,])])
    if (splitByCol) maxes = split(x = maxes, f = names(maxes))
    maxes
}

