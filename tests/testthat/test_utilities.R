context("Extract GDA related data")

# @Todo: Create own test data
data(poison)
res <- MCA(poison[,3:8],excl=c(1,3), graph = FALSE)

test_that(".get_eigenvalues extracts values", {
  expect_equal(length(.get_eigenvalues(res)), nrow(res$eig))
})

context("Create plots")

test_that(".create_plot init a empty plot with an axes cross", {
  p <- .create_plot()
  vdiffr::expect_doppelganger("A blank plot with an axes cross", p)
})

# context("Finalize plots")
#
# data(tea)
# res <- MCA(tea,quanti.sup=19,quali.sup=20:36, graph = FALSE)
#
# test_that(".finalize_plot applies theme and settings", {
#   p <- .finalize_plot(
#     plot(res),
#     res ,
#     labels = c("Hello", "Hello")
#   )
#   vdiffr::expect_doppelganger("A finalized plot", p)
# })
