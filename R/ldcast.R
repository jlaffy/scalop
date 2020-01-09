#' @title Turn a list into a dataframe
#' @param L a list
#' @rdname ldcast
#' @return a dataframe with as many rows as the longest vector in the list
#' @export
ldcast = function(L, na.rm = FALSE, na.value = NULL) {

    if (!is.list(L)) L = list(L)

    Class = unique(sapply(L, class, simplify = F))
    if (Class %in% c('character', 'factor')) {
        d = as.data.frame(sapply(L, "length<-", max(lengths(L))))
    }

    else {
        L = L[lengths(L) != 0]
        L.df = sapply(L, function(l) reshape2::melt(as.matrix(l))[,-2], simplify = F)
        d = Reduce(function(x, y) suppressWarnings(dplyr::full_join(x, y, by = "Var1")), L.df)
        d = tibble::column_to_rownames(d, 'Var1')
        colnames(d) = names(L)
        d = as.matrix(d)
    }

    d = as.matrix(d)
    if (na.rm) d = d[!apply(d, 1, function(row) any(is.na(row))), ]
    else if (!is.null(na.value)) d[is.na(d)] <- na.value
    as.data.frame(d)
}
