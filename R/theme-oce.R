
#' Base theme useful for 'oce' plots
#'
#' @param ... Passed to [ggplot2::theme_bw()]
#'
#' @return A complete [ggplot2::theme()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(hwy, cty)) +
#'   geom_point() +
#'   theme_oce()
#'
theme_oce <- function(...) {
  ggplot2::theme_bw(...) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
}
