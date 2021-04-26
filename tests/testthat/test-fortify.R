
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
