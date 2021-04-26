
#' Prepare 'oce' objects as data frames for 'ggplot2'
#'
#' @param model An 'oce' object
#' @param ... Additional columns/values to initialize.
#' @return A [tibble::tibble()]
#'
#' @importFrom ggplot2 fortify
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(ctd, package = "oce")
#' fortify(ctd)
#'
fortify.ctd <- function(model, ...) {
  tibble::tibble(!!! model@data, ...)
}

#' @rdname fortify.ctd
#' @export
fortify.section <- function(model, ...) {
  station_meta_names <- c("stationId", "longitude", "latitude", "time")
  station_meta <- tibble::tibble(!!! model@metadata[station_meta_names], ...)
  station_meta$distance <- oce::geodDist(model)
  data_tbl <- lapply(model@data$station, fortify)
  data_tbl_n <- vapply(data_tbl, nrow, integer(1))

  data_tbls <- vctrs::vec_rbind(!!! data_tbl)

  vctrs::vec_cbind(
    vctrs::vec_rep_each(station_meta, data_tbl_n),
    # in the off chance meta fields are also present in the ctd objects,
    # the meta field wins
    data_tbls[setdiff(names(data_tbls), names(station_meta))]
  )
}
