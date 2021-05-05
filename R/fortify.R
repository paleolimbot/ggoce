
#' Prepare 'oce' objects as data frames for 'ggplot2'
#'
#' @param model An 'oce' object
#' @param which Most objects can be summarised as data frames at several
#'   levels of granularity. The default is the most useful for plotting.
#' @param type For adp objects, one of "velocity" (one row per time per
#'   beam per cell), "bottom_track" (one row per time per beam), or
#'   "metadata" (one row per time).
#' @param ... Additional columns/values to initialize. Tidy evaluation
#'   is supported.
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
    tibble::as_tibble(model@data[data_vars])
  } else if (which == "metadata") {
    tibble::as_tibble(model@metadata[meta_vars])
  } else if (which == "combined") {
    data <- tibble::as_tibble(model@data[data_vars])
    meta <- tibble::as_tibble(model@metadata[meta_vars])
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
    station_meta <- tibble::as_tibble(model@metadata[station_meta_names])
    station_meta$distance <- oce::geodDist(model)
    station_meta
  } else if (which == "combined") {
    station_meta <- tibble::as_tibble(model@metadata[station_meta_names])
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

#' @rdname fortify.ctd
#' @export
fortify.adp <- function(model, ..., type = c("velocity", "bottom_track", "metadata"),
                        which = c("data", "metadata", "combined")) {
  type <- match.arg(type)
  which <- match.arg(which)

  n_pings <- length(model@data$time)
  n_distance <- length(model@data$distance)
  n_beams <- model@metadata$numberOfBeams

  distance <- model@data$distance
  beam <- beamName(model, seq_len(n_beams))
  beam <- factor(beam, levels = beam)

  meta_lengths <- vapply(model@metadata, length, integer(1))
  meta_dim <- lapply(model@metadata, dim)
  meta_dim_null <- vapply(meta_dim, is.null, logical(1))
  data_lengths <- vapply(model@data, length, integer(1))
  data_dim <- lapply(model@data, dim)
  data_dim_null <- vapply(data_dim, is.null, logical(1))

  glance_vars <- names(model@metadata)[(meta_lengths == 1) & meta_dim_null]

  meta_vars_data <- names(model@data)[(data_lengths == n_pings) & data_dim_null]
  meta_vars_meta <- names(model@metadata)[(meta_lengths == n_pings) & meta_dim_null]
  meta <- vctrs::vec_cbind(
    tibble::as_tibble(model@data[meta_vars_data]),
    tibble::as_tibble(model@metadata[meta_vars_meta])
  )

  data_is_velocity <- vapply(
    data_dim,
    identical,
    c(n_pings, n_distance, n_beams),
    FUN.VALUE = logical(1)
  )

  data_is_bottom_track <- vapply(
    data_dim,
    identical,
    c(n_pings, n_beams),
    FUN.VALUE = logical(1)
  )

  tbl <- if (type == "metadata" && which == "metadata") {
    tibble::as_tibble(model@metadata[glance_vars])
  } else if (type == "metadata" && which == "combined") {
    vctrs::vec_cbind(
      tibble::as_tibble(model@metadata[glance_vars]),
      meta
    )
  } else if (which == "metadata") {
    meta
  } else if (type == "bottom_track" && which == "data") {
    data <- lapply(model@data[data_is_bottom_track], "dim<-", NULL)
    dims <- expand.grid(time = model@data$time, beam = beam)
    vctrs::vec_cbind(
      tibble::as_tibble(dims),
      if (length(data) > 0) tibble::as_tibble(data)
    )
  } else if (type == "bottom_track" && which == "combined") {
    data <- lapply(model@data[data_is_bottom_track], "dim<-", NULL)
    dims <- expand.grid(time = model@data$time, beam = beam)
    dims$time <- NULL
    vctrs::vec_cbind(
      vctrs::vec_rep(meta, n_beams),
      tibble::as_tibble(dims),
      if (length(data) > 0) tibble::as_tibble(data)
    )
  } else if (type == "velocity" && which == "data") {
    data <- lapply(model@data[data_is_velocity], "dim<-", NULL)
    dims <- expand.grid(time = model@data$time, distance = distance, beam = beam)
    vctrs::vec_cbind(
      tibble::as_tibble(dims),
      if (length(data) > 0) tibble::as_tibble(data)
    )
  } else if (type == "velocity" && which == "combined") {
    data <- lapply(model@data[data_is_velocity], "dim<-", NULL)
    dims <- expand.grid(time = model@data$time, distance = distance, beam = beam)
    dims$time <- NULL
    vctrs::vec_cbind(
      vctrs::vec_rep(meta, n_beams * n_distance),
      tibble::as_tibble(dims),
      if (length(data) > 0) tibble::as_tibble(data)
    )
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
