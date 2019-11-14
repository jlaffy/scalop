#' @title Assign Rows and Sample 
#' @description Assign rows to columns of a matrix with maxcol_strict and subsequently sample from each column's group.
#' @param mat a matrix
#' @param size sample size
#' @param min minimum value required for a row to be assigned to a column. Default: 1
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: 0.5
#' @param replace boolean value indicating whether sample replacement is allowed. Will be set to TRUE with warning if the sample is smaller than the sample size. Default: F
#' @return a list of sampled rows for each column, each elemnent a character vector of size <size>
#' @rdname sample_strict
#' @export 
sample_strict <- function(mat, size, min = 1, diff = 0.5, replace = F) {
    # assigns each row to one of the column variables or to nothing
    # returns rownames by column variable in a list
    groups = maxcol_strict(mat = mat, min = min, diff = diff, splitByCol = TRUE)
    group_size = min(lengths(groups))

    if (group_size < size) {
        print(paste0('Group size < ', size))
        print(paste0('size reset to: ', group_size))
        size = group_size
    }
    
    # sample from the resulting 'snobby' assignments
    sapply(groups, sample, size = size, replace = replace, simplify = F)
}

