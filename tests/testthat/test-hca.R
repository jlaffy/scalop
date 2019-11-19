context('hierarchical clustering analysis')

m = rowcenter(scdata[1:50, 1:10])

test_that("NULL cor.method skips correlation step", {
    Names = names(hca(m, cor.method = NULL))
    expect_false("cr" %in% Names)
})

test_that("ordered correlation matrices are equal", {
    cr1 = hca_cor(m, reorder = T)
    cr2 = hca_reorder(hca_cor(m, reorder = F))
    obj = .hca(m, hclust.end = T)
    cr3 = obj$cr[obj$ord, obj$ord]
    expect_identical(cr1, cr2)
    expect_identical(cr1, cr3)
    cr1 = hca_cor(m, reorder.row = F)
    cr2 = hca_reorder(hca_cor(m, reorder = F), row = F)
    expect_identical(cr1, cr2)
})

test_that("order output is the same from matrix and correlation matrix", {
    ord1 = .hca(m, hclust.end = T)$order
    ord2 = hca(hca_cor(m, reorder = F), cor.method = NULL)$ord
    expect_identical(ord1, ord2)
})

test_that("hca_cor warns if given a correlation matrix", {
    expect_warning(hca_cor(hca_cor(m)))
})

test_that("hca_dist warns if matrix diagonal does not equal max.dist", {
    cr = hca_cor(m)
    expect_warning(hca_dist(cr, max.dist = 20))
})

test_that("is_cor recognises correlation matrix", {
    cr = matrix(rnorm(mean = 0, sd = 0.1, n = 9), ncol = 3)
    diag(cr) = 1
    expect_true(is_cor(cr))
})

