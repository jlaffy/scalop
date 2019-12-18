#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param m numeric matrix. The matrix should not contain NA values.
#' @param group the group to be tested. It should be a character vector or a two-level factor in which the first corresponds to the group.
#' @param group2 the group to test against. It should be a character vector or a two-level factor in which the first corresponds to the group and the second to group2.
#' @param lfc log2-fold change cutoff; numeric value or NULL. Default: log2(2)
#' @param p (adjusted) p-value cutoff; numeric value between 0 and 1 or NULL. Default: 0.05
#' @param pmethod correction method for multiple testing or 'none' if not desired. See stats::p.adjust.methods for options. Default: 'BH'
#' @param arrange.by arrange genes by decreasing 'lfc', increasing 'p' or 'none. Default: 'lfc'
#' @param return.val 'lfc', 'df', 'gene' or 'p'. Default: 'lfc' 
#' @param center.rows should the matrix rows be centered around 0? If TRUE and group2 was provided, centering is performed after the matrix has been subset to include only group and group2 columns. Default: F
#' @param two.way should the reverse dea test be performed too? i.e. group becomes group2 and vice versa. Default: FALSE
#' @return a numeric vector of gene fold changes or p-values, a character vector of gene names or the full dataframe.
#' @rdname dea
#' @export 
dea = function(m,
               group,
               group2 = NULL,
               lfc = log2(2),
               p = 0.05,
               pmethod = 'BH',
               arrange.by = c('lfc', 'p', 'none'),
               return.val = c('lfc', 'df', 'gene', 'p'),
               center.rows = FALSE,
               two.way = FALSE) {

    .dea = function(m,
                    group,
                    group2,
                    lfc,
                    p,
                    pmethod,
                    arrange.by,
                    return.val,
                    center.rows) {
    
        if (!is.null(group2)) {
            columns = unique(c(group, group2))
            m = m[, columns]
        }
        if (center.rows) m = rowcenter(m)
        print(range_rowmeans(m))
        d = rowttests(m, fac = group)
        d = as.data.frame(d)
        d = tibble::rownames_to_column(d, 'gene')
        d$p.value = stats::p.adjust(d$p.value, method = pmethod)
        if (!is.null(lfc)) d = dplyr::filter(d, foldchange >= lfc)
        if (!is.null(p)) d = dplyr::filter(d, p.value <= p)
        if (arrange.by == 'lfc') d = dplyr::arrange(d, desc(foldchange))
        else if (arrange.by == 'p') d = dplyr::arrange(d, p.value)
        else if (arrange.by != 'none') warning('Skipping arrange.by: value not recognised...')
        if (return.val == 'df') return(d)
        genes = dplyr::pull(d, gene)
        lfc = stats::setNames(dplyr::pull(d, foldchange), genes)
        p = stats::setNames(dplyr::pull(d, p.value), genes)
        if (return.val == 'gene') return(genes)
        else if (return.val == 'lfc') return(lfc)
        else if (return.val == 'p') return(p)
        else stop('return.val not recognised.')
    }

    return.val = match.arg(return.val)
    arrange.by = match.arg(arrange.by)

    Args = list(m = m,
                group = group,
                group2 = group2,
                lfc = lfc,
                p = p,
                pmethod = pmethod,
                arrange.by = arrange.by,
                return.val = return.val,
                center.rows = center.rows)

    res = do.call(.dea, Args)

    if (two.way) {
        Args$group = group2
        Args$group2 = group
        res = c(list(res), list(do.call(.dea, Args)))
    }
    
    res

}

