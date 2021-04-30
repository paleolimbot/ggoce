
test_that("ggplot() with geom_isopycnal() builds", {
  # crude, but makes sure the code to render the plot runs at least once
  tbl <- tibble::tibble(salinity = c(30, 31), temperature = c(4, 14))
  p <- ggplot2::ggplot(tbl, ggplot2::aes(salinity, temperature)) +
    geom_isopycnal() +
    ggplot2::geom_point()
  built <- expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_gtable(built), "gtable")
})

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
  iso_gsw <- isopycnal_isolines(30:31, 4:14, ref_latitude = 45, ref_longitude = -64, eos = "gsw")
  expect_identical(names(iso)[1:5], names(iso_gsw)[1:5])
})

test_that("isopycnal_isolines() works with conservative temperature", {
  # stub to remind me to write this test
})

test_that("isopycnal_isoline() works with absolute salinity", {
  # stub to remind me to write this test
})
