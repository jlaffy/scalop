
#' @title Summary Statistics for Bootstrapped State Fractions
#' @description Compute summary statistics for bootstrapped data
#' @param bootstraps a matrix or dataframe of data values in rows by bootstrap repeats in columns
#' @return dataframe with the mean and SEM across all bootstrap repeats (columns) for each row in <bootstraps>
#' @seealso 
#'  \code{\link[matrixStats]{rowMads}}
#' @rdname bootstrap_summary
#' @export 
#' @importFrom matrixStats rowSds
bootstrap_summary = function(bootstraps) {
    if (is.list(bootstraps)) {
        bootstraps = as.matrix(as.data.frame(bootstraps))
    }
    summary.mean = rowMeans(bootstraps)
    summary.sem = matrixStats::rowSds(bootstraps)/sqrt(ncol(bootstraps))
    data.frame(summary.mean, summary.sem)
}


#' @title Bootstrapped State Fractions 
#' @description The function subsamples from the rows (observations / cells) in <scores> and uses the subsetted matrix to compute state fractions. This process is repeated <n.iter> times with subsamples of <sample.size>.
#' @param scores a dataframe of cell signature scores. Cells are rows and signatures are columns.
#' @param sample.size the size of the subsample to be taken in each bootstrap. Default: 200
#' @param n.iter the number of bootstraps or iterations. Default: 10000
#' @param min minimum value required for a row to be assigned to a column. Default: NULL
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: NULL
#' @param summary logical; return summary statistics for bootstraps instead of the raw data. See `?scalop::bootstrap_summary` for details. Default: F
#' @return a dataframe with as many columns as <n.iter> and as many rows as the number of columns (signatures) in <scores>. if summary = TRUE, instead returns a dataframe with two columns: mean and SEM.
#' @rdname bootstrapped_state_fractions
#' @export 
bootstrapped_state_fractions = function(scores,
                                        sample.size = 200,
                                        n.iter = 1e4L,
                                        min = NULL,
                                        diff = NULL,
                                        summary = F) {

    samples = replicate(n.iter, sample(rownames(scores), sample.size), simplify = F)
    if (is.null(samples)) samples = list(rownames(scores))
    Call = quote(state_fractions(scores[cells, ], min = min, diff = diff))
    bootstraps = sapply(samples, function(cells) eval(Call))
    colnames(bootstraps) = 1:n.iter 

    if (summary) {
        summary = bootstrap_summary(bootstraps)
        return(list(summary = summary, bootstraps = bootstraps))
    }

    bootstraps
}


#' @title Assign Samples to States by High- and Low -frequency of their Cells in each State 
#' @description Samples are assigned to a particular state if the fraction of cells in that state is significantly high. Separately, samples are also assigned to states according to state fractions that are particularly low. Significance is measured against the average state fractions of the population, and this in turn is computed by bootstrapping (from cells across all samples) with <n_iter> repeats.
#' @param scores a dataframe of cell signature scores. Cells are rows and signatures are columns.
#' @param groups list of samples (i.e. rowname subsets in scores) to assign to states. 
#' @param n.iter number of iterations. Default: 1000
#' @param sample.size the sample size used in each iteration Default: 200
#' @param p p-value Default: 0.001
#' @param min minimum value required for a row to be assigned to a column. Default: NULL
#' @param diff minimum difference in value to the 'next-best' row that is required for a row to be assigned to a column. Default: NULL
#' @param return.cells logical value; return cell ids instead of sample ids? Default: FALSE
#' @param bootstraps if provided, will be used instead of computing bootstrapped repeats. Should be a dataframe with the same rownames as colnames in <scores> and with as many columns as there are bootstrap repeats. Default: NULL
#' @return a list of sample assignments to states. The list is organised by state, and nested for state-high and state-low samples.
#' @rdname assign_bulk_states
#' @export 
assign_bulk_states = function(scores,
                              groups,
                              n.iter = 1e3L,
                              sample.size = 200,
                              p = 1e-3,
                              min = NULL,
                              diff = NULL,
                              return.cells = FALSE,
                              bootstraps = NULL) {

    data = state_fractions(scores,
                           groups = groups,
                           min = min,
                           diff = diff)

    if (is.null(bootstraps)) {
        bootstraps = bootstrapped_state_fractions(scores,
                                                  n.iter = n.iter,
                                                  sample.size = sample.size,
                                                  min = min,
                                                  diff = diff)
    }

    stateNames = rownames(data)
    sampleNames = colnames(data)
    Call = quote(sapply(sampleNames, function(sample) mean(data[state, sample] <= bootstraps[state, ])))
    probs = sapply(stateNames, function(state) eval(Call)) 
    are.high = probs <= p
    are.low = probs >= 1- p
    state.high = apply(are.high, 2, function(col) rownames(are.high)[col])
    state.low = apply(are.low, 2, function(col) rownames(are.low)[col])

    if (return.cells) {
        state.high = sapply(state.high, function(group) as.character(unlist(groups[group])), simplify = F)
        state.low = sapply(state.low, function(group) as.character(unlist(groups[group])), simplify = F)
    }

    result = Map(function(x, y) list(high = x, low = y), x = state.high, y = state.low)
    result
}
