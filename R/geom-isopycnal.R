
#' Draw automagic isopycnals
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return A [ggplot2::layer()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(ctd, package = "oce")
#'
#' ggplot(ctd, aes(salinity, temperature)) +
#'   geom_isopycnal() +
#'   geom_point()
#'
geom_isopycnal <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIsopycnal,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @rdname geom_isopycnal
#' @export
GeomIsopycnal <- ggplot2::ggproto(
  "GeomIsopycnal",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(x = NA_real_, y = NA_real_),

  draw_panel = function(data, panel_params, coord) {
    iso <- isopycnal_isolines(panel_params$x.range, panel_params$y.range)
    iso <- lapply(iso, coord$transform, panel_params)

    isoband::isolines_grob(
      iso,
      breaks = as.numeric(names(iso)),
      units = "native"
    )
  }
)

#' Generate an 'isolines' object representing isopycnals
#'
#' @inheritParams oce::swRho
#' @param trim_freezing Don't draw contours outside the freezing region.
#' @param n_sal Number of points with which contours should be approximated
#'   on the salinity axis.
#' @param n_temp Number of points with which contours should be approximated
#'   on the temperature axis.
#' @param breaks A numeric vector or function used to generate breaks
#'   from objects.
#' @param n_breaks Passed to `breaks` if `breaks` is a function.
#'
#' @return An object returned by [isoband::isolines()]. The `x` values
#'   of the output refer to `salinity` and the `y` values of the output
#'   refer to temperature.
#' @export
#'
#' @examples
#' data(ctd, package = "oce")
#' isolines <- isopycnal_isolines(ctd[["salinity"]], ctd[["temperature"]])
#'
#' plot(ctd[["salinity"]], ctd[["temperature"]])
#' for (line in isolines) {
#'   lines(line)
#' }
#'
isopycnal_isolines <- function(salinity, temperature,
                               trim_freezing = TRUE,
                               breaks = pretty,
                               n_breaks = 5,
                               n_sal = 200, n_temp = 200,
                               eos = getOption("oceEOS", default = "gsw")) {
  sal_range <- range(salinity, finite = TRUE)
  temp_range <- range(temperature, finite = TRUE)

  # not configurable in oce::plotTS() for contours and maybe
  # referencePressure is the wrong term
  referencePressure <- 0

  sal_value <- seq(sal_range[1], sal_range[2], length.out = n_sal)
  temp_value <- seq(temp_range[1], temp_range[2], length.out = n_temp)

  # create grid containing density values
  sal_grid <- vctrs::vec_rep_each(sal_value, n_temp)
  temp_grid <- vctrs::vec_rep(temp_value, n_sal)
  if (identical(eos, "gsw")) {
    rho_grid <- gsw::gsw_rho(sal_grid, temp_grid, referencePressure) - 1000
  } else {
    rho_grid <- oce::swSigma(sal_grid, temp_grid, referencePressure, eos = "unesco")
  }

  if (trim_freezing) {
    # technically using unesco even if eos is "gsw"
    # to avoid location requirement (also in oce::plotTS())
    is_freezing <- temp_grid < oce::swTFreeze(sal_grid, referencePressure, eos = "unesco")
    rho_grid[is_freezing] <- NA_real_
  }

  # calculate breaks
  if (is.numeric(breaks)) {
    levels <- breaks
  } else {
    breaks <- rlang::as_function(breaks, env = parent.frame())
    levels <- breaks(rho_grid, n_breaks)
  }

  # use isoband::isolines() to calculate values
  dim(rho_grid) <- c(n_temp, n_sal)
  isoband::isolines(
    sal_value, temp_value, rho_grid,
    levels = levels
  )
}
