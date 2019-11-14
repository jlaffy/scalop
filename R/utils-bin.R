
.check_arg <- function(arg) {
# x should contain one character vector only.
    if (is.list(arg) & all(lengths(arg) >= 1)) {
        stop('Please provide a single character vector.')
    }
}

.arg_is_args <- function(arg) {
# TRUE if there is more than one char. vector in arg
    arg = ifelse (is.list(arg) & all(lengths(arg) >= 1), TRUE, FALSE)
    return(arg)
}

.check_sample_size <- function(bins, n, replace) {
    err1 = c('Error: Group is smaller than sample size <n> and <replace> == F.')
    err2 = c('Set <replace> = TRUE to proceed (or make <n> smaller).')
    errors <- c(err1, err2)
    if (any(lengths(bins) < n) & replace == FALSE) {
        stop(cat(errors, sep = '\n'))
    }
}

.name_binIDs <- function(binIDs, x) {
    if (!is.null(names(x))) {
        names(binIDs) = names(x)
    } else {
        names(binIDs) = x 
    }
    return(binIDs)
}

.check_missing <- function(Group, ref) {
# all values in x should be in bins.
    if (is.numeric(ref)) ref = names(ref)
    are_missing = !Group %in% ref
    err = 'Error: Returning missing name(s)...'
    if (any(are_missing)) {
        missing = Group[are_missing]
        stop(cat(c(err, missing), sep = '\n'))
    }
}

.check_args_exist <- function(x, data, bins) {
    args = list(x, data, bins)
    err1 = 'Not enough arguments. Provide one of:'
    err2 = '\t1. <data> : data to be transformed to <x>'
    err3 = '\t2. <x> : vector to be binned'
    err4 = '\t3. <bins>: bin vector for control <Group>'
    errors = c(err1, err2, err3, err4)
    if (all(sapply(args, is.null))) {
        stop(cat(errors, sep = '\n'))
    }
}

