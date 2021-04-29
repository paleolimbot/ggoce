
test_that("isopycnal_isolines() works", {
  iso <- isopycnal_isolines(30:31, 4:14)
  expect_s3_class(iso, "isolines")
  iso_df <- do.call(rbind, lapply(iso, as.data.frame))
  lengths <- vapply(iso, function(x) length(x[[1]]), integer(1))
  iso_df$rho <- vctrs::vec_rep_each(as.numeric(names(iso)), lengths)
  expect_true(all(iso_df$x >= 30 & iso_df$x <= 31))
  expect_true(all(iso_df$y >= 4 & iso_df$y <= 14))
  expect_true(all(iso_df$rho >= 22.5 & iso_df$rho <= 24.5))
})

test_that("isopycnal_isolines() can use gsw", {
  iso <- isopycnal_isolines(30:31, 4:14)
  iso_gsw <- isopycnal_isolines(30:31, 4:14, eos = "gsw")
  expect_identical(names(iso)[1:5], names(iso_gsw)[1:5])
})

