
#' Draw automagic isopycnals
#'
#' Draws density contour lines ("isopycnals") as a ggplot2 layer.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams isopycnal_isolines
#' @param lineend,linejoin,linemitre,colour,size,linetype,alpha Customize
#'   the appearance of contour lines. See [ggplot2::geom_path()] for details.
#' @param text.size,family Customize the appearance of contour line labels.
#'   See [ggplot2::geom_text()] for details.
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
geom_isopycnal <- function(mapping = NULL, data = NULL,
                           ...,
                           lineend = "butt", linejoin = "round", linemitre = 10,
                           colour = "darkgray", size = 0.4, linetype = 1, alpha = NA,
                           text.size = 3, family = "",
                           salinity_type = c("practical", "absolute"),
                           temperature_type = c("in-situ", "conservative"),
                           ref_pressure = 0,
                           ref_longitude = NULL, ref_latitude = NULL,
                           trim_freezing = TRUE,
                           breaks = pretty,
                           n_breaks = 5,
                           n_sal = 200, n_temp = 200,
                           eos = getOption("oceEOS", default = "gsw"),
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  salinity_type <- match.arg(salinity_type)
  temperature_type <- match.arg(temperature_type)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomIsopycnal,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      colour = colour, size = size, linetype = linetype, alpha = alpha,
      text.size = text.size, family = family,
      salinity_type = salinity_type,
      temperature_type = temperature_type,
      ref_pressure = ref_pressure,
      ref_longitude = ref_longitude,
      ref_latitude = ref_latitude,
      trim_freezing = trim_freezing,
      breaks = breaks,
      n_breaks = n_breaks,
      n_sal = n_sal,
      n_temp = n_temp,
      eos = eos
    )
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

  draw_panel = function(data, panel_params, coord,
                        lineend, linejoin, linemitre, colour, size, linetype, alpha,
                        text.size, family,
                        salinity_type, temperature_type,
                        ref_pressure, ref_latitude, ref_longitude,
                        trim_freezing,
                        breaks, n_breaks, n_sal, n_temp, eos) {
    # note: really need to backtransform range here
    iso <- isopycnal_isolines(
      panel_params$x.range, panel_params$y.range,
      ref_pressure = ref_pressure,
      ref_longitude = ref_longitude,
      ref_latitude = ref_latitude,
      salinity_type = salinity_type,
      temperature_type = temperature_type,
      trim_freezing = trim_freezing,
      breaks = breaks,
      n_breaks = n_breaks,
      n_sal = n_sal,
      n_temp = n_temp,
      eos = eos
    )

    # transform back to coordinate space
    iso <- lapply(iso, coord$transform, panel_params)

    # use the isolines grob to do the heavy lifting
    isoband::isolines_grob(
      iso,
      gp = grid::gpar(
        col = colour,
        fontsize = text.size * ggplot2::.pt,
        fontfamily = family,
        lwd = size * ggplot2::.pt,
        lty = linetype,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      ),
      breaks = as.numeric(names(iso)),
      units = "native"
    )
  }
)

#' Generate an 'isolines' object representing isopycnals
#'
#' @param salinity,salinity_type A salinity value. Use
#'   `salinity_type = "absolute"` to specify that `salinity` values
#'   are absolute salinities.
#' @param temperature,temperature_type A temperature value. Use
#'   `temperature_type = "conservative"`
#'   to specify that these values are not in-situ temperature.
#' @param ref_pressure The pressure that should be used to calculate
#'   density contours in dbar.
#' @param ref_longitude,ref_latitude The latitude and longitude that should
#'   be used in seawater calculations if `eos` is "gsw".
#' @param trim_freezing Don't draw contours outside the freezing region.
#' @param n_sal Number of points with which contours should be approximated
#'   on the salinity axis.
#' @param n_temp Number of points with which contours should be approximated
#'   on the temperature axis.
#' @param breaks A numeric vector or function used to generate breaks
#'   from objects.
#' @param n_breaks Passed to `breaks` if `breaks` is a function.
#' @param eos The equation of state: one of "unesco" or "gsw". See
#'   [oce::swRho()] for details.
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
                               ref_pressure = 0,
                               ref_longitude = NULL,
                               ref_latitude = NULL,
                               salinity_type = c("practical", "absolute"),
                               temperature_type = c("in-situ", "conservative"),
                               trim_freezing = TRUE,
                               breaks = pretty,
                               n_breaks = 5,
                               n_sal = 200, n_temp = 200,
                               eos = getOption("oceEOS", default = "gsw")) {

  salinity_type <- match.arg(salinity_type)
  temperature_type <- match.arg(temperature_type)

  # usually this will be a range already, but can be useful to
  # pass in an arbitrary vector of values as well.
  sal_range <- range(salinity, finite = TRUE)
  temp_range <- range(temperature, finite = TRUE)

  # It's important that these values are on a regular grid
  # since they'll be used for coordinates of the contour lines
  # and need to match the types of input `salinity` and `temperature`.
  sal_value <- seq(sal_range[1], sal_range[2], length.out = n_sal)
  temp_value <- seq(temp_range[1], temp_range[2], length.out = n_temp)

  # Make sure we have the right salinity and temperature types to calculate
  # the in-situ density. To accommodate a mix of types, make sure we have
  # practical salinity and in-situ temperature calculated.
  sal_grid <- vctrs::vec_rep_each(sal_value, n_temp)
  temp_grid <- vctrs::vec_rep(temp_value, n_sal)

  if (salinity_type == "absolute") {
    assert_has_lonlat(ref_longitude, ref_latitude)

    sal_grid_absolute <- sal_grid
    sal_grid_practical <- gsw::gsw_SP_from_SA(
      SA = sal_grid,
      p = ref_pressure,
      longitude = ref_longitude,
      latitude = ref_latitude
    )
  } else if(temperature_type == "conservative") {
    assert_has_lonlat(ref_longitude, ref_latitude)

    sal_grid_practical <- sal_grid
    sal_grid_absolute <- gsw::gsw_SA_from_SP(
      SP = sal_grid,
      p = ref_pressure,
      longitude = ref_longitude,
      latitude = ref_latitude
    )
  } else {
    sal_grid_practical <- sal_grid
    sal_grid_absolute <- NULL
  }

  if (temperature_type == "conservative") {
    temp_grid_in_situ <- gsw::gsw_t_from_CT(sal_grid_absolute, temp_grid, ref_pressure)
  } else {
    temp_grid_in_situ <- temp_grid
  }

  # using in-situ density
  rho_grid <- oce::swRho(
    salinity = sal_grid_practical,
    temperature = temp_grid_in_situ,
    pressure = ref_pressure,
    longitude = ref_longitude,
    latitude = ref_latitude
  ) - 1000

  if (trim_freezing) {
    is_freezing <- temp_grid_in_situ < oce::swTFreeze(
      salinity = sal_grid_practical,
      pressure = ref_pressure,
      longitude = ref_longitude,
      latitude = ref_latitude,
      eos = eos
    )
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

assert_has_lonlat <- function(ref_longitude, ref_latitude) {
  stopifnot(
    !is.null(ref_longitude), length(ref_longitude) == 1, is.numeric(ref_longitude),
    !is.null(ref_latitude), length(ref_latitude) == 1, is.numeric(ref_latitude),
  )
}
