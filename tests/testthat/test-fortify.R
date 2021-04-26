
test_that("fortify() works for ctd objects", {
  a <- oce::as.ctd(
    salinity = 35 + 1:3 / 10,
    temperature = 10 - 1:3 / 10,
    pressure = 1:3
  )

  expect_identical(
    fortify(a),
    tibble::tibble(
      scan = 1:3,
      salinity = 35 + 1:3 / 10,
      temperature = 10 - 1:3 / 10,
      pressure = 1:3
    )
  )
})

test_that("fortify() works for section objects", {
  data(section, package = "oce", envir = environment())
  tbl <- fortify(section)

  # check that metadata was added in the right order
  expect_identical(
    tbl$stationId[1:6],
    c("3", "3", "3", "3", "3", "4")
  )
})
