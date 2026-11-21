test_that("app launcher returns the delegated app value", {
  if (isTRUE(as.logical(Sys.getenv("NOT_CRAN")))) skip("CRAN core delegation check")
  skip_if_not_installed("xplorerr")
  sentinel <- new.env(parent = emptyenv())
  local_mocked_bindings(app_vistributions = function() sentinel, .package = "xplorerr")
  expect_identical(vdist_launch_app(), sentinel)
})
