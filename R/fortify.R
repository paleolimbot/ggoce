
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
fortify.ctd <- function(model, ..., which = c("data", "combined", "metadata")) {
  which <- match.arg(which)

  meta_lengths <- vapply(model@metadata, length, integer(1))
  meta_vars <- names(model@metadata)[meta_lengths == 1]

  data_lengths <- vapply(model@data, length, integer(1))
  data_vars <- names(model@data)[data_lengths == length(model@data[[1]])]

  tbl <- if (which == "data") {
    tibble::tibble(!!! model@data[data_vars])
  } else if (which == "metadata") {
    tibble::tibble(!!! model@metadata[meta_vars])
  } else if (which == "combined") {
    data <- tibble::tibble(!!! model@data[data_vars])
    meta <- tibble::tibble(!!! model@metadata[meta_vars])
    vctrs::vec_cbind(meta, data)
  } else {
    stop(sprintf("Unsupported value for `which`: '%s'", which), call. = FALSE) # nocov
  }

  tbl_apply_dots(tbl, ...)
}

#' @rdname fortify.ctd
#' @export
fortify.section <- function(model, ..., which = c("combined", "metadata", "data")) {
  which <- match.arg(which)

  # not all metadata fields relate to the station and some are not
  # subset consistently (but stationId always is, so base the fields
  # on that)
  station_id_length <- length(model@metadata$stationId)
  metadata_length <- vapply(model@metadata, length, integer(1))
  station_meta_names <- setdiff(
    names(model@metadata)[metadata_length == station_id_length],
    c("header", "filename", "sectionId")
  )

  tbl <- if (which == "data") {
    data_tbl <- lapply(model@data$station, fortify, which = "data")
    vctrs::vec_rbind(!!! data_tbl)
  } else if (which == "metadata") {
    station_meta <- tibble::tibble(!!! model@metadata[station_meta_names])
    station_meta$distance <- oce::geodDist(model)
    station_meta
  } else if (which == "combined") {
    station_meta <- tibble::tibble(!!! model@metadata[station_meta_names])
    station_meta$distance <- oce::geodDist(model)

    data_tbl <- lapply(model@data$station, fortify, which = "data")
    data_tbl_n <- vapply(data_tbl, nrow, integer(1))
    data_tbls <- vctrs::vec_rbind(!!! data_tbl)

    vctrs::vec_cbind(
      vctrs::vec_rep_each(station_meta, data_tbl_n),
      # in the off chance meta fields are also present in the ctd objects,
      # the meta field wins
      data_tbls[setdiff(names(data_tbls), names(station_meta))]
    )
  } else {
    stop(sprintf("Unsupported value for `which`: '%s'", which), call. = FALSE) # nocov
  }

  tbl_apply_dots(tbl, ...)
}

# basically mutate()
tbl_apply_dots <- function(tbl, ...) {
  dots <- lapply(rlang::enquos(...), rlang::eval_tidy, data = tbl)
  if (length(dots) > 0) {
    vctrs::vec_cbind(tbl, tibble::as_tibble(dots))
  } else {
    tbl
  }
}
