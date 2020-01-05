.check_missing <- function(Group, ref) {
# all values in x should be in bins.
    if (is.numeric(ref)) ref = names(ref)
    are_missing = !Group %in% ref
    err = 'Error: Returning missing name(s)...'
    if (any(are_missing)) {
        missing = Group[are_missing]
        warning(cat(c(err, missing), sep = '\n'))
    }
}

.filter_sigs = function(Group, ref, conserved = 0.5) {
    Group = Group[Group %in% ref]
    if ((length(Group)/length(ref)) < conserved) {
        warning('Too few <group> genes conserved after filtering')
        return(NULL)
    }
    Group
}

.score <- function(mat, groups, controls = NULL, center = FALSE) {

    # Score matrix by groups
    # Scores are the column averages for the subset of rows specified in groups
    # Optional: Control group(s) scores to subtract from the real scores

    # Input check
    .check_missing(Group = unique(unlist(groups)), ref = rownames(mat))
    groups = sapply(groups, function(Group) .check_retained,Group = Group, ref= rownames(mat), simplify = F)
    
    # Score without controls
    if (is.null(controls) | isFALSE(controls)) {
        s.mat = sapply(groups, function(p) colMeans(mat[p, ]))
    }

    # Score with controls
    else {
        if (isTRUE(controls)) {
            # Default control (all rows)
            control.vals = colMeans(mat)
            control.call = quote(control.vals[i])
        }
        else {
            # User-provided controls
            .check_missing(Group = unique(unlist(controls)), ref = rownames(mat))
            control.call = quote(colMeans(mat[controls[[i]], , drop = F]))
        }
        # Score
        groupids = stats::setNames(1:length(groups), names(groups))
        score.call = quote(colMeans(mat[groups[[i]], , drop = F]))
        s.mat = sapply(groupids, function(i) eval(score.call) - eval(control.call))
    }

    # Center?
    if (center) {
        s.mat = colcenter(s.mat)
    }

    return(s.mat)
}


#' @title Score matrix columns for groups of rows 
#' @description This function has been deprecated and replaced with scalop::sigScores. Please see `?scalop::sigScores` for details.
# #' @description Score a matrix by groups of rows. Scores are column averages for the subset of rows specified in a group. Option to generate control group scores to subtract from the real scores. Control groups can either be user-provided or generated from binning of the rows. Similarly, the bins themselves can be user-provided or computed.
# #' @param mat an expression matrix of gene rows by cell columns.
# #' @param groups a character vector or list of character vectors. Each character vector is a group or signature to score each column against and should match subsets of rownames in <mat>.
# #' @param binmat an expression matrix of gene rows by cell columns that will be used to create the gene bins and ultimately the control signatures for correction of the cell scores. For our use cases, <mat> and <binmat> are identical except that the former is row-centered and used to generate cell scores and the latter is not row-centered and used to correct the cell scores. If NULL, and bin.control = T (and neither <bins> nor <controls> were provided), <mat> will be used. Careful that in this use case <mat> should not be row-centered for the correction to be meaningful. Default: NULL
# #' @param bins a named character vector with as names the rownames and as values the ID of each bin. You can provide the bins directly (e.g. with bin()) rather than these being generated from <binmat>. Default: NULL
# #' @param controls. A character vector if <groups> is a character vector a list of character vectors of the same length as <groups>. Each character vector is a control signature whose genes should have expression levels similar to those in the corresponding real signature, but be otherwise biologically meaningless. You can provide the control signatures directly (e.g. with binmatch()) rather than these being generated from <binmatch> / <bins>. Default: NULL
# #' @param bin.control boolean value. If your controls can be generated straight from <mat> (i.e. if mat is not row-centered and you do not provide <binmatch>, <bins>, or <controls>), then you can just call score(mat, groups, bin.control = TRUE). Default: F
# #' @param center boolean value. Should the resulting score matrix be column-centered? This option should be considered if binned controls are not used. Default: F
# #' @param nbin numeric value specifying the number of bins. Not relevant if <bins> or <controls> are provided on input. Default is 30, but please be aware that we chose 30 bins for ~ 8500 genes and if your # of genes is very different you should consider changing this. Default: 30
# #' @param n numeric value for the number of control genes to be sampled per gene in a signature. Not relevant if <controls> is provided on input. Default: 100
# #' @param replace boolean value. Allow bin sampling to be done with replacement. Default: F
# #' @return a matrix with as rows the columns of the input matrix and as columns the scores of each group provided in groups. If one group is provided, the matrix returned will have 1 column.

#' @rdname score
#' @export 
score <- function(mat,
                  groups,
                  binmat = NULL,
                  bins = NULL,
                  controls = NULL,
                  bin.control = F,
                  center = F,
                  nbin = 30,
                  n = 100,
                  replace = F) {

    message('This function has been deprecated and replaced with scalop::sigScores...')
    message('See `?scalop::sigScores` for details.')
    return(NULL)
    # Wrapper for .score() with option to first generate control groups
    # Controls are generated with binmatch()

    # Generate controls if bins provided
    if (!is.null(bins|binmat)) {
        bin.control = TRUE
    }

    # Except if controls already provided
    if (!is.null(controls)) {
        bin.control = FALSE
    } 

    if (bin.control && is.null(binmat) && all(unique(round(rowMeans(mat), 3)) == 0)) {
        message('Warning: if <mat> is row-centered and <binmat> was not provided, your bins will not be meaningful.')
    }
    # Get bin controls
    if (bin.control) {
        # Make bins if not provided
        if (is.null(bins)) {
            if (!is.null(binmat)) {
                bins = bin(mat = binmat, breaks = nbin)
            } else {
                bins = bin(mat = mat, breaks = nbin)
            }
        }

        # Make controls for groups using bins
        if (.arg_is_args(arg = groups)) {
            # If <groups> is many:
            controls = sapply(groups,
                              binmatch,
                              bins = bins,
                              n = n,
                              replace = replace,
                              simplify = F)
        } else {
            # If <groups> is one:
            controls = binmatch(Group = groups,
                                   bins = bins,
                                   n = n,
                                   replace = replace) }
    }

    # Score (with controls or without if controls = NULL)
    res = .score(mat = mat, groups = groups, controls = controls, center = center)
    as.data.frame(res)
}
