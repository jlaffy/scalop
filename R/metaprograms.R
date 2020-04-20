
#' @title Compute Metaprograms 
#' @description programs and profiles are the lists of gene sets and DE profiles that were derived from intra-tumour cell clusters. These are integrated, according to the metaclusters clustering, into a final list of metaprograms; one per metacluster.
#' @param programs list of differentially expressed genes in each group/cell cluster
#' @param profiles list of differential expression profiles for all genes in each group/cell cluster
#' @param m matrix of genes by cells
#' @param metaclusters list of program metaclusters 
#' @param samples character vector of unique sample names. Default: readRDS("samples_2020-03-01.rds")
#' @param freq minimum number of metacluster-specific sample-specific programs a gene must appear in. Default: 3
#' @param order.priority order by 'freq' first or 'lfc' first. Default: 'freq'
#' @return if return.data = F, just the list of metaprograms. Else, the list of metaprograms, and the dataframe of metacluster-, sample- and gene- specific LFC and freq values.
#' @details DETAILS
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{join}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{arrange}}
#' @rdname metaprograms
#' @export 
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate full_join right_join group_by ungroup distinct select filter arrange
metaprograms = function(programs,
                        profiles,
                        m,
                        metaclusters,
                        samples = NULL,
                        freq.cutoff = 3,
                        order.priority = c('freq', 'lfc'),
                        return.data = FALSE) {
    library(scalop)

    mp = metaclusters
    sigs = programs
    lfc = profiles
    
    # bit of a hack
    if (is.null(samples)) samples = names(programs)

    # data with 'allowed' genes per metaprogram
    gmp = data.frame(gene = unlist(sigs),
                     program = rep(names(sigs), lengths(sigs)),
                     stringsAsFactors = F)
    
    # data with programs per metaprogram
    mp = data.frame(mp = rep(names(mp),
                             lengths(mp)),
                    program = as.character(unlist(mp)),
                    stringsAsFactors = F)
    
    # data with log fold change values per program
    lfc = do.call(cbind.data.frame, lfc)
    lfc = reshape2::melt(lfc %>% as.matrix) %>%
        dplyr::rename(gene = Var1, program = Var2, lfc = value) %>%
        dplyr::mutate(gene = as.character(gene), program = as.character(program))
    
    # combine all dataframes
    df = dplyr::full_join(mp, lfc) %>% dplyr::right_join(gmp)
    df = dplyr::mutate(df, sample = str_extract(program, paste0(samples, collapse = "|")))
    df = df %>% dplyr::group_by(mp, sample, gene) %>% dplyr::mutate(tum.lfc = mean(lfc))
    df = df %>% dplyr::ungroup() %>% dplyr::distinct(mp, sample, gene, tum.lfc)
    df = df %>% dplyr::group_by(mp, gene) %>% dplyr::mutate(freq = n()) 
    df = df %>% dplyr::ungroup()
    df = df %>% dplyr::group_by(mp, gene) %>% dplyr::mutate(mp.lfc = mean(tum.lfc))
    df = df %>% dplyr::ungroup() %>% dplyr::select(-sample, -tum.lfc) %>% dplyr::distinct()
    df = df %>% dplyr::filter(freq >= freq.cutoff)

    if (match.arg(order.priority) == 'freq') {
        df = df %>% dplyr::arrange(mp, desc(freq), desc(mp.lfc))
    } else {
        df = df %>% dplyr::arrange(mp, desc(mp.lfc))
    }

    mp = split(df$gene, df$mp) 
    if (!return.data) return(mp)
    list(mp = mp, data = df)
}
