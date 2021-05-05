
test_that("fortify() works for ctd objects", {
  a <- oce::as.ctd(
    salinity = 35 + 1:3 / 10,
    temperature = 10 - 1:3 / 10,
    pressure = 1:3
  )

  expect_identical(
    fortify(a, which = "data"),
    tibble::tibble(
      scan = 1:3,
      salinity = 35 + 1:3 / 10,
      temperature = 10 - 1:3 / 10,
      pressure = 1:3
    )
  )

  expect_identical(
    fortify(a, which = "metadata"),
    tibble::tibble(
      pressureType = "sea",
      deploymentType = "unknown",
      waterDepth = NA,
      filename = "",
      type = "",
      serialNumber = ""
    )
  )

  # without user-supplied dots
  expect_identical(
    fortify(a, which = "combined"),
    vctrs::vec_cbind(
      tibble::tibble(
        pressureType = "sea",
        deploymentType = "unknown",
        waterDepth = NA,
        filename = "",
        type = "",
        serialNumber = ""
      ),
      tibble::tibble(
        scan = 1:3,
        salinity = 35 + 1:3 / 10,
        temperature = 10 - 1:3 / 10,
        pressure = 1:3
      )
    )
  )

  # with user-supplied dots
  expect_identical(
    fortify(a, salinity2 = salinity * 2, which = "combined"),
    vctrs::vec_cbind(
      tibble::tibble(
        pressureType = "sea",
        deploymentType = "unknown",
        waterDepth = NA,
        filename = "",
        type = "",
        serialNumber = ""
      ),
      tibble::tibble(
        scan = 1:3,
        salinity = 35 + 1:3 / 10,
        temperature = 10 - 1:3 / 10,
        pressure = 1:3
      ),
      tibble::tibble(
        salinity2 = (35 + 1:3 / 10) * 2
      )
    )
  )
})

test_that("fortify() works for section objects", {
  data(section, package = "oce", envir = environment())

  expect_identical(
    fortify(section, which = "metadata")$stationId,
    section[["stationId"]]
  )

  expect_identical(
    fortify(section, which = "data")$salinity,
    unlist(lapply(section@data$station, "[[", "salinity"))
  )

  # check that metadata was added in the right order for which = "combined"
  expect_identical(
    fortify(section, which = "combined")$stationId[1:6],
    c("3", "3", "3", "3", "3", "4")
  )
})

test_that("fortify() works for adp objects", {
  data(adp, package = "oce", envir = environment())

  expect_identical(nrow(fortify(adp, type = "metadata", which = "metadata")), 1L)
  expect_identical(nrow(fortify(adp, type = "metadata", which = "data")), 25L)
  expect_identical(nrow(fortify(adp, type = "metadata", which = "combined")), 25L)

  expect_identical(nrow(fortify(adp, type = "bottom_track", which = "metadata")), 25L)
  expect_identical(nrow(fortify(adp, type = "bottom_track", which = "data")), 25L * 4L)
  expect_identical(nrow(fortify(adp, type = "bottom_track", which = "combined")), 25L * 4L)

  expect_identical(nrow(fortify(adp, type = "velocity", which = "metadata")), 25L)
  expect_identical(nrow(fortify(adp, type = "velocity", which = "data")), 25L * 4L * 84L)
  expect_identical(nrow(fortify(adp, type = "velocity", which = "combined")), 25L * 4L * 84L)
})
