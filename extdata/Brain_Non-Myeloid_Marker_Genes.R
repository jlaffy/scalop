m0 = readr::read_csv('FACS/Brain_Non-Myeloid-counts.csv')
annotation0 = readr::read_csv('annotations_facs.csv')

# set gene rownames
# note re the ~2k gene ~names suffixed with 'Rik':
# https://bioinformatics.stackexchange.com/questions/3205/what-are-riken-genes
# they are new genes without proper gene names yet
m = as.data.frame(m0)
rownames(m) = as.character(unlist(m[, 1]))
m = m[, -1]
m.cpm = scalop::cpm(as.matrix(m))
m.logcpm = scalop::logcpm(m.cpm, bulk = FALSE)

# subset cells so that all are annotated with a cell type
annotation = dplyr::filter(annotation0, !is.na(cell_ontology_class))
cells = intersect(colnames(m.logcpm), annotation$cell)
annotation = dplyr::filter(annotation, cell %in% cells)
m.logcpm = m.logcpm[, cells]
# the matrix is already pre-processed for cells with complexity >= 500 genes
m.logcpm = m.logcpm[scalop::aggr_gene_expr(m.logcpm) >= 4, ]
# 3401 cells that are HQ with cell_ontology_class annotation
# 7734 mouse genes with aggregate expression (log2-space) >= 4
# matrix in log(CPM/10+1) form
saveRDS(m.logcpm, 'Brain_Non-Myeloid.logcpm.ss2.rds')
saveRDS(m[rownames(m.logcpm), colnames(m.logcpm)], 'Brain_Non-Myeloid.counts.ss2.rds')

# group cells by cell_ontology_group annotation
# astrocyte, neuron, oligodendrocyte, oligodendrocyte precursor, pericyte, endothelial, or bergmann glial cell
cell_type_groups = split(annotation$cell, annotation$cell_ontology_class)
lengths(cell_type_groups)
saveRDS(cell_type_groups, 'Brain_Non-Myeloid.CellTypeGroups.rds')

dea_against_each = scalop::dea(m = m.logcpm,
                               group = cell_type_groups,
                               group2 = cell_type_groups,
                               lfc = log2(2),
                               p = 0.005,
                               return.val = 'lfc',
                               center.rows = T,
                               arrange.by = 'lfc')

# adjusted p-value of 0.005 means that 1 in 200 DE genes will be a false positive
final = sapply(dea_against_each, scalop:::combine_group2_dea, simplify = F)
final = sapply(final, function(g) head(toupper(g), 100), simplify = F)
Markers_Normal = load('../data/Markers_Normal.rda')
Markers_Normal = c(Markers_Normal, final)
usethis::use_data(Markers_Normal, overwrite = T)

