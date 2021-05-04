
#' Draw automagic isopycnals
#'
#' Draws density contour lines ("isopycnals") as a ggplot2 layer. The
#' `x` axis is assumed to be salinity; the `y` axis is assumed to be
#' temperature. To use this geometry as standalone layer, you will need
#' to specify which type of temperature (i.e., in-situ or conservative)
#' and salinity (i.e., practical or absolute) are on these axes.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams isopycnal_isolines
#' @param labels A function or vector of labels with which isopycnals
#'   should be labeled.
#' @param label_placer See [label_placer_isopycnal()] and
#'   [isoband::label_placer_minmax()] for ways to customize
#'   label placement. Use `NULL` for a default that avoids data passed
#'   to the layer.
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
                           temperature_type = c("in-situ", "potential", "conservative"),
                           ref_pressure = 0,
                           ref_longitude = NULL, ref_latitude = NULL,
                           trim_freezing = TRUE,
                           breaks = pretty,
                           labels = identity,
                           label_placer = label_placer_isopycnal(),
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
      labels = labels,
      label_placer = label_placer,
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
                        labels, label_placer,
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

    # transform data to coordinate space if we need it for the label_placer
    data[c("x", "y")] <- coord$transform(data[c("x", "y")], panel_params)

    # calculate breaks and labels
    breaks <- names(iso)
    if (is.character(labels) || is.numeric(labels)) {
      labels <- as.character(labels)
    } else {
      labels <- rlang::as_function(labels)
      labels <- labels(breaks)
    }

    # check lengths
    if (length(breaks) != length(labels)) {
      stop("`length(breaks)` must equal `length(labels)`", call. = FALSE)
    }

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
      breaks = breaks,
      labels = labels,
      label_placer = label_placer,
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
                               temperature_type = c("in-situ", "potential", "conservative"),
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
  } else if(temperature_type %in% c("potential", "conservative")) {
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

  # I can't find a way to "uncompute" potential temperature, so treating
  # as identical to conservative temperature for now
  if (temperature_type %in% c("potential", "conservative")) {
    temp_grid_in_situ <- gsw::gsw_t_from_CT(sal_grid_absolute, temp_grid, ref_pressure)
  } else {
    temp_grid_in_situ <- temp_grid
  }

  # using in-situ density rho (could export density function
  # as an option)
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

#' Isopycnal label placement
#'
#' Automatic labels placement is difficult and infrequently perfect. The
#' default strategy to place labels uses a similar strategy to
#' [isoband::label_placer_minmax()] except coordinates are constrained
#' to a fraction of the visible area to keep labels from disappearing
#' outside the plot bounds.
#'
#' @param padding A proportion of the plot (x, y) that should be excluded
#'   on all sides when choosing label center points. Ignored if
#'   `bounds` is specified.
#' @param bounds A vector of xmin, ymin, xmax, ymax, outside which
#'   label centers should not exist.
#' @param placement A string containing one or more of "t", "b", "l",
#'   or "r". See [isoband::label_placer_minmax()].
#'
#' @return A function suitable as input to [geom_isopycnal()]'s
#'   `label_placer` argument.
#' @export
#'
label_placer_isopycnal <- function(
  placement = "t",
  padding = c(0.025, 0.025),
  bounds = c(padding[1], padding[2], 1 - padding[1], 1 - padding[2])
) {
  force(bounds)
  minmax_placer <- isoband::label_placer_minmax(placement = placement)

  function(lines, labels_data) {
    # clip xy to bounds
    lines <- lapply(lines, function(l) {
      within_bounds <-
        (l$x >= bounds[1]) &
        (l$x <= bounds[3]) &
        (l$y >= bounds[2]) &
        (l$y <= bounds[4])

      lapply(l, "[", within_bounds)
    })

    # remove tiny lines where the label might cover the whole line
    line_length <- vapply(lines, function(l) {
      n <- length(l$x)
      if (n < 2) {
        return(0)
      }

      sum(sqrt((l$x[-n] - l$x[-1]) ^ 2 + (l$y[-n] - l$y[-1]) ^ 2))
    }, double(1))

    # hardcoding a threshold of 20% of the plot
    labels_data <- labels_data[line_length > 0.2, , drop = FALSE]

    minmax_placer(lines, labels_data)
  }
}

# nocov start
label_placer_debug <- function(label_placer = label_placer_isopycnal()) {
  force(label_placer)

  function(lines, labels_data) {
    browser()
    result <- label_placer(lines, labels_data)
    result
  }
}
# nocov end

assert_has_lonlat <- function(ref_longitude, ref_latitude) {
  stopifnot(
    !is.null(ref_longitude), length(ref_longitude) == 1, is.numeric(ref_longitude),
    !is.null(ref_latitude), length(ref_latitude) == 1, is.numeric(ref_latitude)
  )
}
