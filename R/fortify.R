
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
