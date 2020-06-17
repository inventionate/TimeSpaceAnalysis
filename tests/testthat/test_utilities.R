context("Extract GDA related data")
library(TimeSpaceAnalysis)
# @Todo: Create own test data

data(poison)
res <- MCA (poison[,3:8],excl=c(1,3))

test_that(".get_eigenvalues extracts values", {
  expect_equal(length(.get_eigenvalues(res)), nrow(res$eig))
})

context("Create plots")

test_that(".create_plot init a empty plot with an axes cross", {
  p <- .create_plot()
  vdiffr::expect_doppelganger("A blank plot with an axes cross", p)
})
