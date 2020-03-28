

#' @title State Fractions (by group) 
#' @description Get the fraction of cells in each state, by group if <groups> provided. Here, the rows in <scores> are interpreted as cells and the columns as their state scores, but more generally the function assigns each row to its maximum column (state) and subsequently counts the number of observations per state where if <groups> provided then the observations are first subsetted by group. 
#' @param scores a data.frame object of cell signature scores (cells X signatures/states)
#' @param groups should fractions be computed separately for groups, e.g. samples? a list of observations by group. Default: NULL
#' @param min minimum value required for a row to be assigned to a column. Default: NULL
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: NULL
#' @return a numeric or data.frame object with states as (col)names and if data.frame, groups as rownames.
#' @seealso 
#'  \code{\link[stats]{setNames}}
#' @rdname state_fractions
#' @export 
#' @importFrom stats setNames
state_fractions = function(scores,
                           groups = NULL,
                           min = NULL,
                           diff = NULL) {

    .states = function(scores, min, diff) {
        assignments = maxcol_strict(scores,
                                    min = min,
                                    diff = diff,
                                    splitByCol = TRUE)
        data = lengths(assignments)/sum(lengths(assignments))
        data0 = stats::setNames(rep(0, ncol(scores)), colnames(scores))
        data0[names(data)] <- data
        data0
    }

    if (!is.null(groups)) {
        scores.list = sapply(groups, function(cells) scores[cells, ], simplify = F)
        data = as.data.frame(sapply(scores.list, .states, min = min, diff = diff, simplify = F))
    }
    
    else data = .states(scores, min = min, diff = diff)

    data
}

