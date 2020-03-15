#' @title Find cell clusters and retrieve significant expresion programs 
#' @description Cell clusters are filtered on the basis of size, their corresponding expression programs are subsequently filtered on the basis of number of genes with sufficiently high fold-change values and sufficiently small p-values, and lastly filtered on the basis of sufficiently low jaccard similarity between any two pairs of cell clusters, whereby the cell cluster with a larger number of significant genes is kept. 
#' @param m expression matrix of genes by cells. not row-centered
#' @param groups cell clusters provided and will not be computed from data.
#' @param nsig1 minimum number of genes with p-value 'p' to keep a cluster. Default: 50
#' @param nsig2 number of genes with p-value 'p'/10. Default: 10
#' @param jaccard allowed jaccard similarity between clusters of cells. Default: 0.7
#' @param p DEA adjusted p value for genes. Default: 0.01
#' @param lfc DEA log2-FC value for genes. Default: log2(2)
#' @param pmethod adjust method. Default: 'BH'
#' @return The cell clusters, their gene expression profiles (all genes log2-foldchanges), their top DE genes (n = nsig1) and their p-values are all returned.
#' @rdname programs
#' @export 
programs = function(m,
                    groups = NULL,
                    nsig1 = 50,
                    nsig2 = 10,
                    jaccard = 0.7,
                    p = 0.01,
                    lfc = log2(2), 
                    pmethod = 'BH') {

    library(scalop)
    if (is.null(groups)) {
        groups = hca_groups(rowcenter(m))
    }

    deas = dea(m, 
               group = groups, 
               return.val = 'df', 
               p = p, 
               lfc = lfc, 
               arrange.by = 'lfc',
               pmethod = pmethod) 

    sig1 = sapply(deas, function(df) sum(df$p.adj <= 0.01, na.rm = TRUE))
    sig2 = sapply(deas, function(df) sum(df$p.adj <= 0.001, na.rm = TRUE))
    sig3 = sapply(deas, function(df) sum(df$p.adj <= 0.0001, na.rm = TRUE))
    bool = sig1 >= nsig1 & sig2 >= nsig2
    deas = deas[bool]
    groups = groups[bool]
    sig1 = sig1[bool]
    sig2 = sig2[bool]
    sig3 = sig3[bool]
    ord = order(sig1, sig2, sig3, decreasing = T)
    deas = deas[ord]
    groups = groups[ord]
    sig1 = sig1[ord]
    sig2 = sig2[ord]
    sig3 = sig3[ord]
    jac.pass = jacFilt(groups, threshold = jaccard, which = TRUE)
    deas = deas[jac.pass]
    groups = groups[jac.pass]
    sig1 = sig1[jac.pass]
    sig2 = sig2[jac.pass]
    sig3 = sig3[jac.pass]
    programs = sapply(deas, function(d) d$gene[1:nsig1], simplify = F)
    lfcs = dea(m,
               group = groups,
               return.val = 'lfc',
               arrange.by = 'none',
               p = NULL,
               lfc = NULL, 
               pmethod = pmethod)
    list(programs = programs,
         profiles = lfcs,
         groups = groups,
         deas = deas,
         sig1 = sig1,
         sig2 = sig2,
         sig3 = sig3)
}

