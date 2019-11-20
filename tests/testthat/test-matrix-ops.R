test_that("is matrix after split", {
    c(m1,m2) %<-% split_matrix(m, sample(colnames(m), 50))
    expect_is(m1, 'matrix')
    expect_is(m2, 'matrix')
})

